{-# LANGUAGE RecordWildCards, PatternGuards, ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.Core.Database(
    Trace(..),
    Database, withDatabase, assertFinishedDatabase,
    listDepends, lookupDependencies,
    Ops(..), build, Depends,
    progress,
    Stack, emptyStack, topStack, showStack, showTopStack,
    toReport, checkValid, listLive
    ) where

import Development.Shake.Classes
import General.Binary
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Storage
import Development.Shake.Internal.Types
import Development.Shake.Internal.Special
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Core.Monad
import General.String
import General.Intern as Intern

import Numeric.Extra
import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Control.Concurrent.Extra
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified General.Ids as Ids
import Data.Typeable.Extra
import Data.IORef.Extra
import Data.Maybe
import Data.List
import Data.Either.Extra
import System.Time.Extra
import Data.Monoid
import Prelude

type Map = Map.HashMap


---------------------------------------------------------------------
-- UTILITY TYPES

newtype Step = Step Word32 deriving (Eq,Ord,Show,Binary,NFData,Hashable,Typeable)

incStep (Step i) = Step $ i + 1


---------------------------------------------------------------------
-- CALL STACK

data Stack = Stack (Maybe Key) [Id] !(Set.HashSet Id)

showStack :: Database -> Stack -> IO [String]
showStack Database{..} (Stack _ xs _) = withLock lock $ do
    forM (reverse xs) $ \x ->
        maybe "<unknown>" (show . fst) <$> Ids.lookup status x

addStack :: Id -> Key -> Stack -> Stack
addStack x key (Stack _ xs set) = Stack (Just key) (x:xs) (Set.insert x set)

showTopStack :: Stack -> String
showTopStack = maybe "<unknown>" show . topStack

topStack :: Stack -> Maybe Key
topStack (Stack key _ _) = key

checkStack :: [Id] -> Stack -> Maybe Id
checkStack new (Stack _ old set)
    | bad:_ <- filter (`Set.member` set) new = Just bad
    | otherwise = Nothing

emptyStack :: Stack
emptyStack = Stack Nothing [] Set.empty


---------------------------------------------------------------------
-- CENTRAL TYPES

data Trace = Trace BS Float Float -- ^ (message, start, end)
    deriving Show

instance NFData Trace where
    rnf (Trace a b c) = rnf a `seq` rnf b `seq` rnf c

type StatusDB = Ids.Ids (Key, Status)
type InternDB = IORef (Intern Key)

-- | Invariant: The database does not have any cycles where a Key depends on itself
data Database = Database
    {lock :: Lock
    ,intern :: InternDB
    ,status :: StatusDB
    ,step :: Step
    ,journal :: Id -> Key -> Result -> IO ()
    ,diagnostic :: IO String -> IO () -- ^ logging function
    ,assume :: Maybe Assume
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
    ,depends :: [[Id]] -- ^ dependencies (don't run them early)
    ,execution :: {-# UNPACK #-} !Float -- ^ how long it took when it was last run (seconds)
    ,traces :: [Trace] -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving Show


newtype Pending = Pending (IORef (Status -> IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error or Ready, with the value you set it to

instance Show Pending where show _ = "Pending"


statusType Ready{} = "Ready"
statusType Error{} = "Error"
statusType Loaded{} = "Loaded"
statusType Waiting{} = "Waiting"
statusType Missing{} = "Missing"

isWaiting Waiting{} = True; isWaiting _ = False
isReady Ready{} = True; isReady _ = False


-- All the waiting operations are only valid when isWaiting
type Waiting = Status

afterWaiting :: Waiting -> (Status -> IO ()) -> IO ()
afterWaiting (Waiting (Pending p) _) act = modifyIORef' p (\a s -> a s >> act s)

newWaiting :: Maybe Result -> IO Waiting
newWaiting r = do ref <- newIORef $ \_ -> return (); return $ Waiting (Pending ref) r

runWaiting :: Waiting -> Status -> IO ()
runWaiting (Waiting (Pending p) _) r = ($ r) =<< readIORef p

-- | Wait for a set of actions to complete.
--   If the action returns True, the function will not be called again.
--   If the first argument is True, the thing is ended.
waitFor :: [(a, Waiting)] -> (Bool -> a -> Status -> IO Bool) -> IO ()
waitFor ws@(_:_) act = do
    todo <- newIORef $ length ws
    forM_ ws $ \(k,w) -> afterWaiting w $ \s -> do
        t <- readIORef todo
        when (t /= 0) $ do
            b <- act (t == 1) k s
            writeIORef' todo $ if b then 0 else t - 1


getResult :: Status -> Maybe Result
getResult (Ready r) = Just r
getResult (Loaded r) = Just r
getResult (Waiting _ r) = r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

newtype Depends = Depends {fromDepends :: [Id]}
    deriving (NFData)


data Ops = Ops
    {stored :: Key -> IO (Maybe Value)
        -- ^ Given a Key, find the value stored on disk
    ,equal :: Key -> Value -> Value -> EqualCost
        -- ^ Given both Values, see if they are equal and how expensive that check was
    ,execute :: Stack -> Key -> Capture (Either SomeException (Value, [Depends], Seconds, [Trace]))
        -- ^ Given a stack and a key, either raise an exception or successfully build it
    }

type Returns a = forall b . (a -> IO b) -> (Capture a -> IO b) -> IO b


internKey :: InternDB -> StatusDB -> Key -> IO Id
internKey intern status k = do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            writeIORef' intern is
            Ids.insert status i (k,Missing)
            return i

queryKey :: StatusDB -> Id -> IO (Maybe (Key, Status))
queryKey status i = Ids.lookup status i


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> Ops -> Stack -> [Key] -> Capture (Either SomeException (Seconds,Depends,[Value]))
build pool database@Database{..} Ops{..} stack ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern status

        whenJust (checkStack is stack) $ \bad -> do
            -- everything else gets thrown via Left and can be Staunch'd
            -- recursion in the rules is considered a worse error, so fails immediately
            let Stack _ xs _ = stack
            stack <- forM (reverse $ bad:xs) $ \x ->
                maybe "<unknown>" (show . fst) <$> Ids.lookup status x
            (tk, tname) <- do
                v <- Ids.lookup status bad
                return $ case v of
                    Nothing -> (Nothing, Nothing)
                    Just (k,_) -> (Just $ typeKey k, Just $ show k)
            errorRuleRecursion stack tk tname

        buildMany stack is
            (\v -> case v of Error e -> Just e; _ -> Nothing)
            (\v -> case v of
                Left e -> return $ continue $ Left e;
                Right rs -> do rs <- rs; return $ continue $ Right (0, Depends is, map result rs)) $
            \go -> do
                time <- offsetTime
                go $ \x -> case x of
                    Left e -> addPoolHighPriority pool $ continue $ Left e
                    Right rs -> do rs <- rs; addPoolMediumPriority pool $ do dur <- time; continue $ Right (dur, Depends is, map result rs)
                return $ return ()
    where
        (#=) :: Id -> (Key, Status) -> IO Status
        i #= (k,v) = do
            diagnostic $ do
                old <- Ids.lookup status i
                return $ maybe "Missing" (statusType . snd) old ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) old
            Ids.insert status i (k,v)
            return v

        atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s

        buildMany :: Stack -> [Id] -> (Status -> Maybe a) -> Returns (Either a (IO [Result]))
        buildMany stack is test fast slow = do
            vs <- mapM (reduce stack) is
            let errs = mapMaybe test vs
            if not $ null errs then
                fast $ Left $ head errs
             else if all isReady vs then
                fast $ Right $ return [r | Ready r <- vs]
             else slow $ \slow ->
                waitFor (filter (isWaiting . snd) $ zip is vs) $ \finish i s -> case s of
                    _ | Just e <- test s -> do
                        slow $ Left e -- on error make sure we immediately kick off our parent
                        return True
                    Ready{}
                        | finish -> do
                            slow $ Right $
                                forM is $ \i -> do
                                    Ready r <- snd . fromJust <$> Ids.lookup status i
                                    return r
                            return True
                        | otherwise -> return False

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
                Just (k, Loaded r) -> do
                    let out b = diagnostic $ return $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)
                    let continue r = out True >> check stack i k r (depends r)
                    let rebuild = out False >> run stack i k (Just r)
                    case assume of
                        Just AssumeDirty -> rebuild
                        Just AssumeSkip -> continue r
                        _ -> do
                            s <- stored k
                            case s of
                                Just s -> case equal k (result r) s of
                                    NotEqual -> rebuild
                                    EqualCheap -> continue r
                                    EqualExpensive -> do
                                        -- warning, have the db lock while appending (may harm performance)
                                        r <- return r{result=s}
                                        journal i k r
                                        i #= (k, Loaded r)
                                        continue r
                                _ -> rebuild
                Just (k, res) -> return res

        run :: Stack -> Id -> Key -> Maybe Result -> IO Waiting
        run stack i k r = do
            w <- newWaiting r
            addPoolLowPriority pool $ do
                let reply res = do
                        ans <- withLock lock $ do
                            ans <- i #= (k, res)
                            runWaiting w res
                            return ans
                        case ans of
                            Ready r -> do
                                diagnostic $ return $ "result " ++ atom k ++ " = "++ atom (result r) ++
                                             " " ++ (if built r == changed r then "(changed)" else "(unchanged)")
                                journal i k r -- we leave the DB lock before appending
                            Error _ -> do
                                diagnostic $ return $ "result " ++ atom k ++ " = error"
                            _ -> return ()
                let norm = execute (addStack i k stack) k $ \res ->
                        reply $ case res of
                            Left err -> Error err
                            Right (v,deps,(doubleToFloat -> execution),traces) ->
                                let c | Just r <- r, equal k (result r) v /= NotEqual = changed r
                                      | otherwise = step
                                in Ready Result{result=v,changed=c,built=step,depends=map fromDepends deps,..}

                case r of
                    Just r | assume == Just AssumeClean -> do
                            v <- stored k
                            case v of
                                Just v -> reply $ Ready r{result=v}
                                Nothing -> norm
                    _ -> norm
            i #= (k, w)

        check :: Stack -> Id -> Key -> Result -> [[Id]] -> IO Status
        check stack i k r [] =
            i #= (k, Ready r)
        check stack i k r (ds:rest) = do
            buildMany (addStack i k stack) ds
                (\v -> case v of
                    Error _ -> Just ()
                    Ready dep | changed dep > built r -> Just ()
                    _ -> Nothing)
                (\v -> if isLeft v then run stack i k $ Just r else check stack i k r rest) $
                \go -> do
                    self <- newWaiting $ Just r
                    go $ \v ->
                        if isLeft v then do
                            b <- run stack i k $ Just r
                            afterWaiting b $ runWaiting self
                        else do
                            res <- check stack i k r rest
                            if not $ isWaiting res
                                then runWaiting self res
                                else afterWaiting res $ runWaiting self
                    i #= (k, self)


---------------------------------------------------------------------
-- PROGRESS

progress :: Database -> IO Progress
progress Database{..} = do
    xs <- Ids.toList status
    return $! foldl' f mempty $ map (snd . snd) xs
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
    status <- Ids.toList status
    let bad = [key | (_, (key, Waiting{})) <- status]
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
resultsOnly mp = Map.map (\(k, v) -> (k, let Just r = getResult v in r{depends = map (filter (isJust . flip Map.lookup keep)) $ depends r})) keep
    where keep = Map.filter (isJust . getResult . snd) mp

removeStep :: Map Id (Key, Result) -> Map Id (Key, Result)
removeStep = Map.filter (\(k,_) -> k /= stepKey)

toReport :: Database -> IO [ProfileEntry]
toReport Database{..} = do
    status <- removeStep . resultsOnly <$> Ids.toMap status
    let order = let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
                in dependencyOrder shw $ Map.map (concat . depends . snd) status
        ids = Map.fromList $ zip order [0..]

        steps = let xs = Set.toList $ Set.fromList $ concat [[changed, built] | (_,Result{..}) <- Map.elems status]
                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f (k, Result{..}) = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep built
            ,prfChanged = fromStep changed
            ,prfDepends = mapMaybe (`Map.lookup` ids) (concat depends)
            ,prfExecution = floatToDouble execution
            ,prfTraces = map fromTrace traces
            }
            where fromStep i = fromJust $ Map.lookup i steps
                  fromTrace (Trace a b c) = ProfileTrace (unpack a) (floatToDouble b) (floatToDouble c)
    return [maybe (err "toReport") f $ Map.lookup i status | i <- order]


checkValid :: Database -> (Key -> IO (Maybe Value)) -> (Key -> Value -> Value -> EqualCost) -> [(Key, Key)] -> IO ()
checkValid Database{..} stored equal missing = do
    status <- Ids.toList status
    intern <- readIORef intern
    diagnostic $ return "Starting validity/lint checking"

    -- Do not use a forM here as you use too much stack space
    bad <- (\f -> foldM f [] status) $ \seen (i,v) -> case v of
        (key, Ready Result{..}) -> do
            now <- stored key
            let good = maybe False ((==) EqualCheap . equal key result) now
            diagnostic $ return $ "Checking if " ++ show key ++ " is " ++ show result ++ ", " ++ if good then "passed" else "FAILED"
            return $ [(key, result, now) | not good && not (specialAlwaysRebuilds result)] ++ seen
        _ -> return seen
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Old", Just $ show result),("New", Just $ maybe "<missing>" show now)]
                                        | (key, result, now) <- bad])
            ""

    bad <- return [(parent,key) | (parent, key) <- missing, isJust $ Intern.lookup key intern]
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value" else show n ++ " values") ++ " did not have " ++ (if n == 1 then "its" else "their") ++ " creation tracked")
            (intercalate [("",Just "")] [ [("Rule", Just $ show parent), ("Created", Just $ show key)] | (parent,key) <- bad])
            ""

    diagnostic $ return "Validity/lint check passed"


listLive :: Database -> IO [Key]
listLive Database{..} = do
    diagnostic $ return "Listing live keys"
    status <- Ids.toList status
    return [k | (_, (k, Ready{})) <- status]


listDepends :: Database -> Depends -> IO [Key]
listDepends Database{..} (Depends xs) =
    withLock lock $ do
        forM xs $ \x ->
            fst . fromJust <$> Ids.lookup status x

lookupDependencies :: Database -> Key -> IO [Key]
lookupDependencies Database{..} k =
    withLock lock $ do
        intern <- readIORef intern
        let Just i = Intern.lookup k intern
        Just (_, Ready r) <- Ids.lookup status i
        forM (concat $ depends r) $ \x ->
            fst . fromJust <$> Ids.lookup status x


---------------------------------------------------------------------
-- STORAGE

-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> Result
toStepResult i = Result (newValue i) i i [] 0 []

fromStepResult :: Result -> Step
fromStepResult = fromValue . result


withDatabase :: ShakeOptions -> (IO String -> IO ()) -> (Database -> IO a) -> IO a
withDatabase opts diagnostic act = do
    registerWitness (Proxy :: Proxy StepKey) (Proxy :: Proxy Step)
    witness <- currentWitness
    withStorage opts diagnostic witness $ \status journal -> do
        journal <- return $ \i k v -> journal i (k, Loaded v)

        xs <- Ids.toList status
        let mp1 = Intern.fromList [(k, i) | (i, (k,_)) <- xs]

        (mp1, stepId) <- case Intern.lookup stepKey mp1 of
            Just stepId -> return (mp1, stepId)
            Nothing -> do
                (mp1, stepId) <- return $ Intern.add stepKey mp1
                return (mp1, stepId)

        intern <- newIORef mp1
        step <- do
            v <- Ids.lookup status stepId
            return $ case v of
                Just (_, Loaded r) -> incStep $ fromStepResult r
                _ -> Step 1
        journal stepId stepKey $ toStepResult step
        lock <- newLock
        act Database{assume=shakeAssume opts,..}


instance BinaryWith Witness Result where
    putWith ws (Result x1 x2 x3 x4 x5 x6) = putWith ws x1 >> put x2 >> put x3 >> put (BinList $ map BinList x4) >> put (BinFloat x5) >> put (BinList x6)
    getWith ws = (\x1 x2 x3 (BinList x4) (BinFloat x5) (BinList x6) -> Result x1 x2 x3 (map fromBinList x4) x5 x6) <$>
        getWith ws <*> get <*> get <*> get <*> get <*> get

instance Binary Trace where
    put (Trace a b c) = put a >> put (BinFloat b) >> put (BinFloat c)
    get = (\a (BinFloat b) (BinFloat c) -> Trace a b c) <$> get <*> get <*> get

-- | The Status wrapper over Value is to deforest the Map, avoiding one additional traversal.
instance BinaryWith Witness Status where
    putWith ctx (Loaded x) = putWith ctx x
    putWith ctx x = err $ "putWith, Cannot write Status with constructor " ++ statusType x
    getWith ctx = Loaded <$> getWith ctx
