{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Development.Shake.Database(
    Trace(..),
    Database, withDatabase, assertFinishedDatabase,
    listDepends, lookupDependencies, Step, incStep, Result(..), Status(Ready,Error),
    Ops(..), AnalysisResult(..), build, Depends, subtractDepends, finalizeDepends,
    progress,
    Stack, emptyStack, topStack, showStack, showTopStack,
    toReport, checkValid, listLive
    ) where

import GHC.Generics (Generic)
import Development.Shake.Classes
import General.Binary
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
import Data.IORef.Extra
import Data.Maybe
import Data.List
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
    ,step :: Step
    ,journal :: Id -> (Key, Status {- Loaded or Missing -}) -> IO ()
    ,diagnostic :: String -> IO () -- ^ logging function
    }

data Status
    = Ready Result -- ^ I have a value
    | Error SomeException -- ^ I have been run and raised an error
    | Loaded Result -- ^ Loaded from the database
    | Waiting Pending (Maybe Result) -- ^ Currently checking if I am valid or building
    | Missing -- ^ I am only here because I got into the Intern table
      deriving Show

data Result = Result
    {result :: Value -- ^ the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- ^ when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- ^ the step for deciding if it's valid
    ,depends :: Depends -- ^ dependencies (stored in order of appearance)
    ,generatedBy :: Maybe Id -- ^ generator, if this was generated
    ,execution :: {-# UNPACK #-} !Float -- ^ how long it took when it was last run (seconds)
    ,traces :: [Trace] -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving (Show,Generic)

instance NFData Result

newtype Pending = Pending (IORef (IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error or Ready

instance Show Pending where show _ = "Pending"


statusType Ready{} = "Ready"
statusType Error{} = "Error"
statusType Loaded{} = "Loaded"
statusType Waiting{} = "Waiting"
statusType Missing{} = "Missing"

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

-- | Wait for a set of actions to complete.
--   If the action returns True, the function will not be called again.
--   If the first argument is True, the thing is ended.
waitFor :: [(a, Waiting)] -> (Bool -> a -> IO Bool) -> IO ()
waitFor ws@(_:_) act = do
    todo <- newIORef $ length ws
    forM_ ws $ \(k,w) -> afterWaiting w $ do
        t <- readIORef todo
        when (t /= 0) $ do
            b <- act (t == 1) k
            writeIORef' todo $ if b then 0 else t - 1


getResult :: Status -> Maybe Result
getResult (Ready r) = Just r
getResult (Loaded r) = Just r
getResult (Waiting _ r) = r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

data AnalysisResult v = Rebuild | Continue | Update v deriving (Show,Eq,Functor)

data Ops = Ops
    { analyseResult :: Key -> Value -> IO (AnalysisResult Value)
        -- ^ Given a Key and its stored value, determine whether to run it
    , runKey :: Key -> Maybe Result -> Stack -> Step -> Capture Status
        -- ^ Given a Key and its previous result, run it and return the status
    }


internKey :: InternDB -> StatusDB -> Key -> IO Id
internKey intern status k = do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            writeIORef' intern is
            modifyIORef' status $ Map.insert i (k,Missing)
            return i

queryKey :: StatusDB -> Id -> IO (Maybe (Key, Status))
queryKey status i = Map.lookup i <$> readIORef status

-- | Return either an exception (crash), or (how much time you spent waiting, stored keys, the value)
build :: Pool -> Database -> Ops -> Stack -> Maybe String -> [Key] -> Capture (Either SomeException (Seconds,Depends,[Value]))
build pool database@Database{..} Ops{..} stack maybeBlock ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern status

        whenJust (checkStack is stack) $ \bad -> do
            -- everything else gets thrown via Left and can be Staunch'd
            -- recursion in the rules is considered a worse error, so fails immediately
            status <- readIORef status
            let xs = stackIds stack
            stack <- return $ reverse $ map (maybe "<unknown>" (show . fst) . flip Map.lookup status) $ bad:xs
            (tk, tname) <- return $ case Map.lookup bad status of
                Nothing -> (Nothing, Nothing)
                Just (k,_) -> (Just $ typeKey k, Just $ show k)
            errorRuleRecursion stack tk tname

        vs <- mapM (reduce stack) is
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ continue $ Right (0, Depends [is], [result r | Ready r <- vs])
         else if not $ null errs then
            return $ continue $ Left $ head errs
         else do
            time <- offsetTime
            let done x = do
                    case x of
                        Left e -> addPoolPriority pool $ continue $ Left e
                        Right v -> addPool pool $ do dur <- time; continue $ Right (dur, Depends [is], v)
                    return True
            waitFor (filter (isWaiting . snd) $ zip is vs) $ \finish i -> do
                s <- readIORef status
                case Map.lookup i s of
                    Just (_, Error e) -> done $ Left e -- on error make sure we immediately kick off our parent
                    Just (_, Ready{}) | finish -> done $ Right [result r | i <- is, let Ready r = snd $ fromJust $ Map.lookup i s]
                                      | otherwise -> return False
            return $ return ()
    where
        (#=) :: Id -> (Key, Status) -> IO Status
        i #= (k,v) = do
            s <- readIORef status
            writeIORef' status $ Map.insert i (k,v) s
            diagnostic $ maybe "Missing" (statusType . snd) (Map.lookup i s) ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) (Map.lookup i s)
            return v

        atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s
        out k r b = diagnostic $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)

        -- Rules for each eval* function
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must not return Loaded

        reduce :: Stack -> Id -> IO Status
        reduce stack i = do
            s <- queryKey status i
            case s of
                Nothing -> err $ "interned value missing from database, " ++ show i
                Just (k, Missing) -> run stack i k Nothing
                Just (k, Loaded r) -> check stack i k r (fromDepends $ depends r)
                Just (k, res) -> return res

        run :: Stack -> Id -> Key -> Maybe Result -> IO Waiting
        run stack i k r | Just block <- maybeBlock = errorNoApply (typeKey k) (show k) (fmap show r) block
        run stack i k r = do
            w <- newWaiting r
            addPool pool $ do
                let reply res = do
                        ans <- withLock lock $ do
                            ans <- i #= (k, res)
                            runWaiting w
                            return ans
                        case ans of
                            Ready r -> do
                                diagnostic $ "result " ++ atom k ++ " = "++ atom (result r) ++
                                             " " ++ (if built r == changed r then "(changed)" else "(unchanged)")
                                journal i (k, Loaded r) -- we leave the DB lock before appending
                            Error _ -> do
                                diagnostic $ "result " ++ atom k ++ " = error"
                                journal i (k, Missing)
                            _ -> return ()
                runKey k r (addStack i k stack) step reply
            i #= (k, w)

        check :: Stack -> Id -> Key -> Result -> [[Id]] -> IO Status
        check stack i k r [] = do
            ar <- analyseResult k (result r)
            let update r s = do
                  let r' = r{result=s}
                  addPool pool $ journal i (k, Loaded r') -- leave DB lock before appending
                  continue r'
                continue r = out k r True >> i #= (k, Ready r)
            case ar of
                Rebuild -> out k r False >> run stack i k (Just r)
                Continue -> continue r
                Update s -> update r s
        check stack i k r (ds:rest) = do
            vs <- mapM (reduce (addStack i k stack)) ds
            let ws = filter (isWaiting . snd) $ zip ds vs
            if any isError vs || any (> built r) [changed | Ready Result{..} <- vs] then do
                out k r False
                run stack i k $ Just r
             else if null ws then do
                check stack i k r rest
             else do
                self <- newWaiting $ Just r
                waitFor ws $ \finish d -> do
                    s <- readIORef status
                    let buildIt = do
                            out k r False
                            b <- run stack i k $ Just r
                            afterWaiting b $ runWaiting self
                            return True
                    case Map.lookup d s of
                        Just (_, Error{}) -> buildIt
                        Just (_, Ready r2)
                            | changed r2 > built r -> buildIt
                            | finish -> do
                                res <- check stack i k r rest
                                if not $ isWaiting res
                                    then runWaiting self
                                    else afterWaiting res $ runWaiting self
                                return True
                            | otherwise -> return False
                i #= (k, self)


---------------------------------------------------------------------
-- PROGRESS

progress :: Database -> IO Progress
progress Database{..} = do
    s <- readIORef status
    return $ foldl' f mempty $ map snd $ Map.elems s
    where
        g = floatToDouble

        f s (Ready Result{..}) = if step == built
            then s{countBuilt = countBuilt s + 1, timeBuilt = timeBuilt s + g execution}
            else s{countSkipped = countSkipped s + 1, timeSkipped = timeSkipped s + g execution}
        f s (Loaded Result{..}) = s{countUnknown = countUnknown s + 1, timeUnknown = timeUnknown s + g execution}
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
resultsOnly :: Map Id (Key, Status) -> Map Id (Key, Result)
resultsOnly mp = Map.map (\(k, v) -> (k, let Just r = getResult v in r{depends = Depends . map (filter (isJust . flip Map.lookup keep)) . fromDepends $ depends r})) keep
    where keep = Map.filter (isJust . getResult . snd) mp

removeStep :: Map Id (Key, Result) -> Map Id (Key, Result)
removeStep = Map.filter (\(k,_) -> k /= stepKey)

toReport :: Database -> IO [ProfileEntry]
toReport Database{..} = do
    status <- (removeStep . resultsOnly) <$> readIORef status
    let order = let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
                in dependencyOrder shw $ Map.map (concat . fromDepends . depends . snd) status
        ids = Map.fromList $ zip order [0..]

        steps = let xs = Set.toList $ Set.fromList $ concat [[changed, built] | (_,Result{..}) <- Map.elems status]
                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f (k, Result{..}) = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep built
            ,prfChanged = fromStep changed
            ,prfDepends = mapMaybe (`Map.lookup` ids) (concat $ fromDepends depends)
            ,prfExecution = execution
            ,prfTraces = traces
            }
            where fromStep i = fromJust $ Map.lookup i steps
    return [maybe (err "toReport") f $ Map.lookup i status | i <- order]


checkValid :: Database -> [(Key, Key)] -> ([(Key, Status)] -> IO [(Key, Value, String)]) -> IO ()
checkValid Database{..} missing keyCheck = do
    status <- readIORef status
    intern <- readIORef intern
    diagnostic "Starting validity/lint checking"

    bad <- keyCheck $ Map.elems status
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Cached value", Just $ show result),("New value", Just now)]
                                        | (key, result, now) <- bad])
            ""

    bad <- return [(parent,key) | (parent, key) <- missing, isJust $ Intern.lookup key intern]
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
        let Just i = Intern.lookup k intern
        let Just (_, Ready r) = Map.lookup i status
        return . map (fst . fromJust . flip Map.lookup status) . concat . fromDepends $ depends r


---------------------------------------------------------------------
-- STORAGE

-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> Result
toStepResult i = Result (newValue i) i i mempty Nothing 0 []

fromStepResult :: Result -> Step
fromStepResult = fromValue . result


withDatabase :: ShakeOptions -> (String -> IO ()) -> (Database -> IO a) -> IO a
withDatabase opts diagnostic act = do
    registerWitness $ (undefined :: StepKey)
    registerWitness $ (undefined :: Step)
    witness <- currentWitness
    withStorage opts diagnostic witness $ \mp2 journal -> do
        let mp1 = Intern.fromList [(k, i) | (i, (k,_)) <- Map.toList mp2]

        (mp1, stepId) <- case Intern.lookup stepKey mp1 of
            Just stepId -> return (mp1, stepId)
            Nothing -> do
                (mp1, stepId) <- return $ Intern.add stepKey mp1
                return (mp1, stepId)

        intern <- newIORef mp1
        status <- newIORef mp2
        let step = case Map.lookup stepId mp2 of
                        Just (_, Loaded r) -> incStep $ fromStepResult r
                        _ -> initialStep
        journal stepId (stepKey, Loaded $ toStepResult step)
        lock <- newLock
        act Database{..}


instance BinaryWith Witness Result where
    putWith ws (Result x1 x2 x3 x4 x5 x6 x7) = putWith ws x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put (BinFloat x6) >> put (BinList x7)
    getWith ws = (\x1 x2 x3 x4 x5 (BinFloat x6) (BinList x7) -> Result x1 x2 x3 x4 x5 x6 x7) <$>
        getWith ws <*> get <*> get <*> get <*> get <*> get <*> get

instance BinaryWith Witness Status where
    putWith ctx Missing = putWord8 0
    putWith ctx (Loaded x) = putWord8 1 >> putWith ctx x
    putWith ctx x = err $ "putWith, Cannot write Status with constructor " ++ statusType x
    getWith ctx = do i <- getWord8; if i == 0 then return Missing else Loaded <$> getWith ctx
