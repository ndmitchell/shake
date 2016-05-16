{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Development.Shake.Database(
    Trace(..),
    Database, withDatabase, assertFinishedDatabase,
    listDepends, lookupDependencies, Step, incStep, Result(..), LiveResult(..), Status(Ready,Error),
    Ops(..), build, Id, Depends, subtractDepends, finalizeDepends,
    progress,
    Stack, emptyStack, topStack, showStack, showTopStack,
    toReport, checkValid, listLive
    ) where

import GHC.Generics (Generic)
import Development.Shake.Classes
import Development.Shake.Database2
import Development.Shake.Pool
import Development.Shake.Value
import Development.Shake.Errors
import Development.Shake.Storage
import Development.Shake.Types
import Development.Shake.Profile
import Development.Shake.Monad
import General.Intern as Intern

import Numeric.Extra
import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Control.Concurrent.Extra
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import General.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.IORef.Extra
import Data.Dynamic
import Data.Maybe
import Data.List
import Data.Tuple.Extra
import System.Time.Extra
import Data.Monoid
import Prelude

type Map = Map.HashMap


showStack :: Database -> Stack -> IO [String]
showStack Database{..} (stackIds -> xs) = do
    status <- withLock lock $ readIORef status
    return $ reverse $ map (maybe "<unknown>" (show . fst) . flip Map.lookup status) xs

---------------------------------------------------------------------
-- CENTRAL TYPES

type StatusDB = IORef (Map Id (Key, Status))
type InternDB = IORef (Intern Key)

-- | Invariant: The database does not have any cycles where a Key depends on itself
data Database = Database
    {lock :: Lock
    ,intern :: InternDB
    ,status :: StatusDB
    ,oldstatus :: Map Id (Key, DBStatus)
    ,step :: Step
    ,journal :: Id -> (Key, DBStatus) -> IO ()
    ,diagnostic :: String -> IO () -- ^ logging function
    }

data Status
    = Ready LiveResult
    | Error SomeException -- ^ I have been run and raised a fatal error
    | Waiting Pending (Maybe Result) -- ^ Currently building
      deriving Show

type DBStatus = Result -- ^ Loaded from the database

data Result = Result
    {result :: Value -- ^ the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- ^ when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- ^ the step for deciding if it's valid
    ,depends :: Depends -- ^ dependencies (stored in order of appearance)
    ,execution :: {-# UNPACK #-} !Float -- ^ how long it took when it was last run (seconds)
    } deriving (Show,Generic)

data LiveResult = LiveResult
    { resultValue :: Dynamic -- ^ dynamic return value limited to lifetime of the program
    , resultStore :: Result -- ^ persistent database value
    , traces :: [Trace] -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving (Show,Generic)

instance NFData Result

instance NFData LiveResult where
  rnf (LiveResult x1 x2 x3) = rnf x2 `seq` rnf x3

instance Binary Result where
    put (Result x1 x2 x3 x4 x5) = put x1 >> put x2 >> put x3 >> put x4 >> put (BinFloat x5)
    get = (\x1 x2 x3 x4 (BinFloat x5) -> Result x1 x2 x3 x4 x5) <$>
        get <*> get <*> get <*> get <*> get

newtype Pending = Pending (IORef (IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error or Ready

instance Show Pending where show _ = "Pending"


statusType Ready{} = "Ready"
statusType Error{} = "Error"
statusType Waiting{} = "Waiting"

isError Error{} = True; isError _ = False
isWaiting Waiting{} = True; isWaiting _ = False
isReady Ready{} = True; isReady _ = False

-- All the waiting operations are only valid when isWaiting
type Waiting = Status

afterWaiting :: Waiting -> IO () -> IO ()
afterWaiting (Waiting (Pending p) _) act = modifyIORef' p (>> act)

newWaiting :: Maybe Result -> IO Waiting
newWaiting r = do ref <- newIORef $ return (); return $ Waiting (Pending ref) r

runWaiting :: Waiting -> IO ()
runWaiting (Waiting (Pending p) _) = join $ readIORef p

data WaitResult a = WaitAgain (a, Waiting) | Continue | Finish

-- | Wait for a set of actions to complete.
--   If the action returns Finish, the function will not be called again.
--   If the first argument is True, the thing is ended.
waitFor :: [(a, Waiting)] -> (Bool -> a -> IO (WaitResult a)) -> IO ()
waitFor ws@(_:_) act = do
    todo <- newVar $ length ws
    let waitOn (k,w) = afterWaiting w . modifyVar_ todo . ((evaluate =<<).) $ \t ->
            if t == 0 then return 0 else do
                b <- act (t == 1) k
                case b of
                    Finish -> return 0
                    Continue -> return (t - 1)
                    WaitAgain kw -> do
                      waitOn kw
                      return t
    forM_ ws waitOn


getResult :: Status -> Maybe Result
getResult (Ready r) = Just (resultStore r)
getResult (Waiting _ r) = r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

data Ops = Ops
    { runKey :: Id -> Key -> Maybe Result -> Bool -> Stack -> Step -> Capture Status
        -- ^ Given a Key and its previous result and if its dependencies changed, run it and return the status
    }

lookupKey :: Key -> Intern Key -> Maybe Id
lookupKey = Intern.lookup

internKey :: InternDB -> Key -> IO Id
internKey intern k = do
    is <- readIORef intern
    case lookupKey k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            writeIORef' intern is
            return i

queryKey :: StatusDB -> Id -> IO (Maybe (Key, Status))
queryKey status i = Map.lookup i <$> readIORef status

atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s

updateStatus :: Database -> Id -> (Key, Status) -> IO Status
updateStatus Database{..} i (k,v) = do
    s <- readIORef status
    writeIORef' status $ Map.insert i (k,v) s
    diagnostic $ maybe "Missing" (statusType . snd) (Map.lookup i s) ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) (Map.lookup i s)
    case Map.lookup i s of
        Just (_,w@(Waiting _ _)) -> runWaiting w
    return v

reportResult :: Database -> Id -> Key -> Status -> IO ()
reportResult d@Database{..} i k res = do
    ans <- withLock lock $ updateStatus d i (k,res)
    -- we leave the DB lock before appending to the journal
    case ans of
        Ready (resultStore -> r) -> do
            diagnostic $ "result " ++ atom k ++ " = "++ atom (result r) ++
                          " " ++ (if built r == changed r then "(changed)" else "(unchanged)")
            journal i (k, r)
        Error _ -> do
            diagnostic $ "result " ++ atom k ++ " = error"
            -- don't store errors
        _ -> return ()

-- | Return either an exception (crash), or (how much time you spent waiting, interned keys, key results)
-- 'build' first takes the state lock and (on a single thread) performs as many transitions as it can without waiting on a mutex or running any rules.
-- Then it releases the state lock and runs the rules in the thread pool and waits for all of them to finish
-- A build requiring no rules should not result in any thread contention.
build :: Pool -> Database -> Ops -> Stack -> Maybe String -> [Key] -> Capture (Either SomeException (Seconds,Depends,[LiveResult]))
build pool database@Database{..} Ops{..} stack maybeBlock ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern

        whenJust (checkStack is stack) $ \bad -> do
            -- everything else gets thrown via Left and can be Staunch'd
            -- recursion in the rules is considered a worse error, so fails immediately
            status <- readIORef status
            let xs = stackIds stack
            stack <- return $ reverse $ map (maybe "<unknown>" (show . fst) . flip Map.lookup status) $ bad:xs
            let tname = fmap (show.fst) $ Map.lookup bad status
            errorRuleRecursion stack tname

        vs <- mapM (reduce stack) (zip is ks)
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ continue $ Right (0, Depends [is], [r | Ready r <- vs])
         else if not $ null errs then
            return $ continue $ Left $ head errs
         else do
            time <- offsetTime
            let done x = do
                    case x of
                        Left e -> addPoolPriority pool $ continue $ Left e
                        Right v -> addPool pool $ do dur <- time; continue $ Right (dur, Depends [is], v)
                    return Finish
            waitFor (filter (isWaiting . snd) $ zip is vs) $ \finish i -> do
                s <- readIORef status
                case Map.lookup i s of
                    Just (_, w@Waiting{}) -> return (WaitAgain (i,w))
                    Just (_, Error e) -> done $ Left e -- on error make sure we immediately kick off our parent
                    Just (_, Ready{}) | finish -> done $ Right [r | i <- is, let Ready r = snd $ fromJust $ Map.lookup i s]
                                      | otherwise -> return Continue
            return $ return ()
    where
        -- Rules for each eval* function
        -- * Must NOT lock (assumes lock already in place)
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must not return Loaded

        reduce :: Stack -> (Id,Key) -> IO Status
        reduce stack (i,k) = do
            s <- queryKey status i
            case s of
                Just (_, res) -> return res
                Nothing -> case Map.lookup i oldstatus of
                    Just (oldk, r) -> check stack i oldk r (fromDepends $ depends r)
                    Nothing -> run stack i k Nothing False

        -- | Reduce for dependencies loaded from the database
        reduce' :: Stack -> Id -> IO Status
        reduce' stack i = do
            s <- queryKey status i
            case s of
                Just (_, res) -> return res
                Nothing -> case Map.lookup i oldstatus of
                    Just (oldk, r) -> check stack i oldk r (fromDepends $ depends r)
                    Nothing -> do
                      status <- readIORef status
                      let xs = stackIds stack
                      stack <- return $ reverse $ map (maybe "<unknown>" (show . fst) . flip Map.lookup status) $ xs
                      errorNoReference stack (show i)

        out :: Stack -> Id -> Key -> Result -> Bool -> IO Waiting
        out stack i k r b = do
          diagnostic $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)
          run stack i k (Just r) b

        run :: Stack -> Id -> Key -> Maybe Result -> Bool -> IO Waiting
        run stack i k r b | Just block <- maybeBlock = errorNoApply (show k) (fmap show r) block
        run stack i k r b = do
            w <- newWaiting r
            updateStatus database i (k, w)
            addPool pool $ runKey i k r b (addStack i k stack) step (reportResult database i k)
            return w

        check :: Stack -> Id -> Key -> Result -> [[Id]] -> IO Waiting
        check stack i k r [] = out stack i k r True
        check stack i k r (ds:rest) = do
            vs <- mapM (reduce' (addStack i k stack)) ds
            let ws = filter (isWaiting . snd) $ zip ds vs
            if any isError vs || any (> built r) [changed | Ready (LiveResult {resultStore = Result{..}}) <- vs] then do
                out stack i k r False
             else if null ws then do
                check stack i k r rest
             else do
                self <- newWaiting $ Just r
                updateStatus database i (k, self)
                waitFor ws $ \finish d -> do
                    s <- readIORef status
                    let buildIt = do
                            b <- out stack i k r False
                            afterWaiting b $ runWaiting self
                            return Finish
                    case Map.lookup d s of
                        Just (_, Error{}) -> buildIt
                        Just (_, Ready r2)
                            | changed (resultStore r2) > built r -> buildIt
                            | finish -> do
                                res <- check stack i k r rest
                                if not $ isWaiting res
                                    then runWaiting self
                                    else afterWaiting res $ runWaiting self
                                return Finish
                            | otherwise -> return Continue
                        Just (_, w@Waiting{}) -> return (WaitAgain (d,w))
                return self


---------------------------------------------------------------------
-- PROGRESS

progress :: Database -> IO Progress
progress Database{..} = do
    s <- readIORef status
    return $ foldl' f mempty $ map snd $ Map.elems s
    where
        g = floatToDouble

        f s (Ready (LiveResult {resultStore = Result{..}})) = if step == built
            then s{countBuilt = countBuilt s + 1, timeBuilt = timeBuilt s + g execution}
            else s{countSkipped = countSkipped s + 1, timeSkipped = timeSkipped s + g execution}
--        f s (Loaded Result{..}) = s{countUnknown = countUnknown s + 1, timeUnknown = timeUnknown s + g execution}
        f s (Waiting _ r) =
            let (d,c) = timeTodo s
                t | Just Result{..} <- r = let d2 = d + g execution in d2 `seq` (d2,c)
                  | otherwise = let c2 = c + 1 in c2 `seq` (d,c2)
            in s{countTodo = countTodo s + 1, timeTodo = t}
        f s _ = s


---------------------------------------------------------------------
-- QUERY DATABASE

assertFinishedDatabase :: Database -> IO ()
assertFinishedDatabase Database{..} = do
    -- if you have anyone Waiting, and are not exiting with an error, then must have a complex recursion (see #400)
    status <- readIORef status
    let bad = [key | (_, (key, Waiting{})) <- Map.toList status]
    when (bad /= []) $
        errorComplexRecursion (map show bad)


-- | Given a map of representing a dependency order (with a show for error messages), find an ordering for the items such
--   that no item points to an item before itself.
--   Raise an error if you end up with a cycle.
dependencyOrder :: (Eq a, Hashable a) => (a -> String) -> Map a [a] -> [a]
-- Algorithm:
--    Divide everyone up into those who have no dependencies [Id]
--    And those who depend on a particular Id, Dep :-> Maybe [(Key,[Dep])]
--    Where d :-> Just (k, ds), k depends on firstly d, then remaining on ds
--    For each with no dependencies, add to list, then take its dep hole and
--    promote them either to Nothing (if ds == []) or into a new slot.
--    k :-> Nothing means the key has already been freed
dependencyOrder shw status = f (map fst noDeps) $ Map.map Just $ Map.fromListWith (++) [(d, [(k,ds)]) | (k,d:ds) <- hasDeps]
    where
        (noDeps, hasDeps) = partition (null . snd) $ Map.toList status

        f [] mp | null bad = []
                | otherwise = error $ unlines $
                    "Internal invariant broken, database seems to be cyclic" :
                    map ("    " ++) bad ++
                    ["... plus " ++ show (length badOverflow) ++ " more ..." | not $ null badOverflow]
            where (bad,badOverflow) = splitAt 10 [shw i | (i, Just _) <- Map.toList mp]

        f (x:xs) mp = x : f (now++xs) later
            where Just free = Map.lookupDefault (Just []) x mp
                  (now,later) = foldl' g ([], Map.insert x Nothing mp) free

        g (free, mp) (k, []) = (k:free, mp)
        g (free, mp) (k, d:ds) = case Map.lookupDefault (Just []) d mp of
            Nothing -> g (free, mp) (k, ds)
            Just todo -> (free, Map.insert d (Just $ (k,ds) : todo) mp)


-- | Eliminate all errors from the database, pretending they don't exist
resultsOnly :: Map Id (Key, Status) -> Map Id (Key, LiveResult)
resultsOnly mp = Map.map (second f) keep
    where
      keep = Map.filter (isJust . getResult . snd) mp
      f (Ready v) = let r = resultStore v
                        filteredDeps = Depends . map (filter (isJust . flip Map.lookup keep)) . fromDepends $ depends r
                    in
                        v { resultStore = r { depends = filteredDeps } }

removeStep :: Map Id (Key, LiveResult) -> Map Id (Key, LiveResult)
removeStep = Map.filter (\(k,_) -> k /= stepKey)

toReport :: Database -> IO [ProfileEntry]
toReport Database{..} = do
    status <- (removeStep . resultsOnly) <$> readIORef status
    let order = let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
                in dependencyOrder shw $ Map.map (concat . fromDepends . depends . resultStore . snd) status
        ids = Map.fromList $ zip order [0..]

        steps = let xs = Set.toList $ Set.fromList $ concat [[changed, built] | (_,LiveResult { resultStore = Result{..}}) <- Map.elems status]
                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f (k, LiveResult{resultStore=Result{..},..}) = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep built
            ,prfChanged = fromStep changed
            ,prfDepends = mapMaybe (`Map.lookup` ids) (concat $ fromDepends depends)
            ,prfExecution = execution
            ,prfTraces = traces
            }
            where fromStep i = fromJust $ Map.lookup i steps
    return [maybe (err "toReport") f $ Map.lookup i status | i <- order]


checkValid :: Database -> [(Key, Key)] -> ([(Id, (Key, Status))] -> IO [(Key, Value, String)]) -> IO ()
checkValid Database{..} missing keyCheck = do
    status <- readIORef status
    intern <- readIORef intern
    diagnostic "Starting validity/lint checking"

    bad <- keyCheck $ Map.toList status
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Cached value", Just $ show result),("New value", Just now)]
                                        | (key, result, now) <- bad])
            ""

    bad <- return [(parent,key) | (parent, key) <- missing, isJust $ lookupKey key intern]
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value" else show n ++ " values") ++ " did not have " ++ (if n == 1 then "its" else "their") ++ " creation tracked")
            (intercalate [("",Just "")] [ [("Rule", Just $ show parent), ("Created", Just $ show key)] | (parent,key) <- bad])
            ""

    diagnostic "Validity/lint check passed"


listLive :: Database -> IO [Key]
listLive Database{..} = do
    diagnostic "Listing live keys"
    status <- readIORef status
    return [k | (k, Ready{}) <- Map.elems status]


listDepends :: Database -> Depends -> IO [Key]
listDepends Database{..} (Depends xs) =
    withLock lock $ do
        status <- readIORef status
        return . map (fst . fromJust . flip Map.lookup status) . concat $ xs

lookupDependencies :: Database -> Key -> IO [Key]
lookupDependencies Database{..} k =
    withLock lock $ do
        intern <- readIORef intern
        status <- readIORef status
        let Just i = lookupKey k intern
        let Just (_, Ready r) = Map.lookup i status
        return . map (fst . fromJust . flip Map.lookup status) . concat . fromDepends . depends . resultStore $ r


---------------------------------------------------------------------
-- STORAGE

-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> LiveResult
toStepResult i = LiveResult
  { resultValue = err "Step does not have a value"
  , resultStore = Result (encode i) i i mempty 0
  , traces = [] }

fromStepResult :: Result -> Step
fromStepResult = decode . result

withDatabase :: ShakeOptions -> (String -> IO ()) -> (Database -> IO a) -> IO a
withDatabase opts diagnostic act = do
    registerWitness (undefined :: StepKey)
    witness <- currentWitness
    withStorage opts diagnostic witness $ \mp2 journal' -> do
        let journal i (k,v) = journal' (encode i) (encode (runPut $ putKeyWith witness k,v))
            unpack (i,t) = (decode i, case decode t of (k,s) -> (runGet (getKeyWith witness) k, s))
        let oldstatus = Map.fromList . map unpack $ Map.toList mp2
            mp1 = Intern.fromList [(k, i) | (i, (k,_)) <- Map.toList oldstatus]
        intern <- newIORef mp1
        stepId <- internKey intern stepKey
        let step = case Map.lookup stepId oldstatus of
                        Just (_,r) -> incStep . fromStepResult $ r
                        _ -> initialStep
            stepResult = toStepResult step
        status <- newIORef $ Map.singleton stepId (stepKey, Ready stepResult)
        journal stepId (stepKey, resultStore stepResult)
        lock <- newLock
        act Database{..}
