{-# LANGUAGE RecordWildCards, PatternGuards, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.Core.Database(
    Trace(..), newTrace,
    Database, withDatabase, assertFinishedDatabase,
    listDepends, lookupDependencies,
    BuildKey(..), build, Depends,
    Step, Result(..),
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
import Development.Shake.Internal.Options
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Rendezvous
import qualified Data.ByteString.Char8 as BS
import Data.Word
import General.Extra
import qualified General.Intern as Intern
import General.Intern(Id, Intern)

import Numeric.Extra
import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Control.Concurrent.Extra
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified General.Ids as Ids
import Foreign.Storable
import Data.Typeable.Extra
import Data.IORef.Extra
import Data.Maybe
import Data.List
import Data.Tuple.Extra
import Data.Either.Extra
import System.Time.Extra
import Data.Monoid
import Prelude

type Map = Map.HashMap


---------------------------------------------------------------------
-- UTILITY TYPES

newtype Step = Step Word32 deriving (Eq,Ord,Show,Storable,BinaryEx,NFData,Hashable,Typeable)

incStep (Step i) = Step $ i + 1


---------------------------------------------------------------------
-- CALL STACK

-- Invariant: Stack xs set . HashSet.fromList (map fst xs) == set
data Stack = Stack [(Id,Key)] !(Set.HashSet Id)

showStack :: Stack -> [String]
showStack (Stack xs _) = reverse $ map (show . snd) xs

showTopStack :: Stack -> String
showTopStack = maybe "<unknown>" show . topStack

addStack :: Id -> Key -> Stack -> Stack
addStack x key (Stack xs set) = Stack ((x,key):xs) (Set.insert x set)

topStack :: Stack -> Maybe Key
topStack (Stack xs _) = snd <$> listToMaybe xs

checkStack :: [Id] -> Stack -> Maybe (Id,Key)
checkStack new (Stack xs set)
    | bad:_ <- filter (`Set.member` set) new = Just (bad, fromJust $ lookup bad xs)
    | otherwise = Nothing

emptyStack :: Stack
emptyStack = Stack [] Set.empty


---------------------------------------------------------------------
-- TRACE

data Trace = Trace {-# UNPACK #-} !BS.ByteString {-# UNPACK #-} !Float {-# UNPACK #-} !Float -- ^ (message, start, end)
    deriving Show

instance NFData Trace where
    rnf x = x `seq` () -- all strict atomic fields

newTrace :: String -> Double -> Double -> Trace
newTrace msg start stop = Trace (BS.pack msg) (doubleToFloat start) (doubleToFloat stop)

---------------------------------------------------------------------
-- CENTRAL TYPES

type StatusDB = Ids.Ids (Key, Status)
type InternDB = IORef (Intern Key)

-- | Invariant: The database does not have any cycles where a Key depends on itself
data Database = Database
    {lock :: Lock
    ,intern :: InternDB
    ,status :: StatusDB
    ,step :: {-# UNPACK #-} !Step
    ,journal :: Id -> Key -> Result BS.ByteString -> IO ()
    ,diagnostic :: IO String -> IO () -- ^ logging function
    }

data Status
    = Ready (Result Value) -- ^ I have a value
    | Error SomeException -- ^ I have been run and raised an error
    | Loaded (Result BS.ByteString) -- ^ Loaded from the database
    | Waiting (Waiting Status) (Maybe (Result BS.ByteString)) -- ^ Currently checking if I am valid or building
    | Missing -- ^ I am only here because I got into the Intern table
      deriving Show

data Result a = Result
    {result :: a -- ^ the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- ^ when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- ^ the step for deciding if it's valid
    ,depends :: [Depends] -- ^ dependencies (don't run them early)
    ,execution :: {-# UNPACK #-} !Float -- ^ how long it took when it was last run (seconds)
    ,traces :: [Trace] -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving (Show,Functor)


statusType Ready{} = "Ready"
statusType Error{} = "Error"
statusType Loaded{} = "Loaded"
statusType Waiting{} = "Waiting"
statusType Missing{} = "Missing"


getResult :: Status -> Maybe (Result ())
getResult (Ready r) = Just $ void r
getResult (Loaded r) = Just $ void r
getResult (Waiting _ r) = void <$> r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

newtype Depends = Depends {fromDepends :: [Id]}
    deriving NFData

instance Show Depends where
    -- Appears in diagnostic output and the Depends ctor is just verbose
    show = show . fromDepends


newtype BuildKey = BuildKey
    {buildKey
        :: Stack -- Given the current stack with the key added on
        -> Step -- And the current step
        -> Key -- The key to build
        -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
        -> Bool -- True if any of the children were dirty
        -> Capture (Either SomeException (Bool, BS.ByteString, Result Value))
            -- Either an error, or a result.
            -- If the Bool is True you should rewrite the database entry.
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


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> BuildKey -> Stack -> [Key] -> Capture (Either SomeException (Seconds,Depends,[Value]))
build pool Database{..} BuildKey{..} stack ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern status

        whenJust (checkStack is stack) $ \(badId, badKey) ->
            -- everything else gets thrown via Left and can be Staunch'd
            -- recursion in the rules is considered a worse error, so fails immediately
            errorRuleRecursion (showStack stack ++ [show badKey]) (typeKey badKey) (show badKey)

        buildMany stack is
            (\v -> case v of Error e -> Just e; _ -> Nothing)
            (\v -> return $ continue $ case v of
                Left e -> Left e
                Right rs -> Right (0, Depends is, map result rs)) $
            \go -> do
                time <- offsetTime
                go $ \x -> case x of
                    Left e -> addPoolHighPriority pool $ continue $ Left e
                    Right rs -> addPoolMediumPriority pool $ do dur <- time; continue $ Right (dur, Depends is, map result rs)
                return $ return ()
    where
        (#=) :: Id -> (Key, Status) -> IO Status
        i #= (k,v) = do
            diagnostic $ do
                old <- Ids.lookup status i
                return $ maybe "Missing" (statusType . snd) old ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) old
            Ids.insert status i (k,v)
            return v

        buildMany :: Stack -> [Id] -> (Status -> Maybe a) -> Returns (Either a [Result Value])
        buildMany stack is test fast slow = do
            let toAnswer v | Just v <- test v = Abort v
                toAnswer (Ready v) = Continue v
            let toCompute (Waiting w _) = Later $ toAnswer <$> w
                toCompute x = Now $ toAnswer x

            res <- rendezvous =<< mapM (fmap toCompute . reduce stack) is
            case res of
                Now v -> fast v
                Later w -> slow $ \slow -> afterWaiting w slow

        -- Rules for each of the following functions
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must return one of the designated subset of values

        reduce :: Stack -> Id -> IO Status {- Ready | Error | Waiting -}
        reduce stack i = do
            s <- Ids.lookup status i
            case s of
                Nothing -> errorInternal $ "interned value missing from database, " ++ show i
                Just (k, Missing) -> spawn True stack i k Nothing
                Just (k, Loaded r) -> check stack i k r (depends r)
                Just (k, res) -> return res


        -- | Given a Key and the list of dependencies yet to be checked, check them
        check :: Stack -> Id -> Key -> Result BS.ByteString -> [Depends] -> IO Status {- Ready | Waiting -}
        check stack i k r [] = spawn False stack i k $ Just r
        check stack i k r (Depends ds:rest) = do
            let cont v = if isLeft v then spawn True stack i k $ Just r else check stack i k r rest
            buildMany (addStack i k stack) ds
                (\v -> case v of
                    Error _ -> Just ()
                    Ready dep | changed dep > built r -> Just ()
                    _ -> Nothing)
                cont $
                \go -> do
                    (self, done) <- newWaiting
                    go $ \v -> do
                        res <- cont v
                        case res of
                            Waiting w _ -> afterWaiting w done
                            _ -> done res
                    i #= (k, Waiting self $ Just r)


        -- | Given a Key, queue up execution and return waiting
        spawn :: Bool -> Stack -> Id -> Key -> Maybe (Result BS.ByteString) -> IO Status {- Waiting -}
        spawn dirtyChildren stack i k r = do
            (w, done) <- newWaiting
            addPoolLowPriority pool $
                buildKey (addStack i k stack) step k r dirtyChildren $ \res -> do
                    let status = either Error (Ready . thd3) res
                    withLock lock $ do
                        i #= (k, status)
                        done status
                    case res of
                        Right (write, bs, r) -> do
                            diagnostic $ return $
                                "result " ++ showBracket k ++ " = "++ showBracket (result r) ++
                                " " ++ (if built r == changed r then "(changed)" else "(unchanged)")
                            when write $ journal i k r{result=bs}
                        Left _ ->
                            diagnostic $ return $ "result " ++ showBracket k ++ " = error"
            i #= (k, Waiting w r)


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
resultsOnly :: Map Id (Key, Status) -> Map Id (Key, Result ())
resultsOnly mp = Map.map (\(k, v) -> (k, let Just r = getResult v in r{depends = map (Depends . filter (isJust . flip Map.lookup keep) . fromDepends) $ depends r})) keep
    where keep = Map.filter (isJust . getResult . snd) mp

removeStep :: Map Id (Key, Result a) -> Map Id (Key, Result a)
removeStep = Map.filter (\(k,_) -> k /= stepKey)

toReport :: Database -> IO [ProfileEntry]
toReport Database{..} = do
    status <- removeStep . resultsOnly <$> Ids.toMap status
    let order = let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
                in dependencyOrder shw $ Map.map (concatMap fromDepends . depends . snd) status
        ids = Map.fromList $ zip order [0..]

        steps = let xs = Set.toList $ Set.fromList $ concat [[changed, built] | (_,Result{..}) <- Map.elems status]
                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f (k, Result{..}) = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep built
            ,prfChanged = fromStep changed
            ,prfDepends = mapMaybe (`Map.lookup` ids) (concatMap fromDepends depends)
            ,prfExecution = floatToDouble execution
            ,prfTraces = map fromTrace traces
            }
            where fromStep i = fromJust $ Map.lookup i steps
                  fromTrace (Trace a b c) = ProfileTrace (BS.unpack a) (floatToDouble b) (floatToDouble c)
    return [maybe (errorInternal "toReport") f $ Map.lookup i status | i <- order]


checkValid :: Database -> (Key -> Value -> IO (Maybe String)) -> [(Key, Key)] -> IO ()
checkValid Database{..} check missing = do
    status <- Ids.toList status
    intern <- readIORef intern
    diagnostic $ return "Starting validity/lint checking"

    -- Do not use a forM here as you use too much stack space
    bad <- (\f -> foldM f [] status) $ \seen (i,v) -> case v of
        (key, Ready Result{..}) -> do
            good <- check key result
            diagnostic $ return $ "Checking if " ++ show key ++ " is " ++ show result ++ ", " ++ if isNothing good then "passed" else "FAILED"
            return $ [(key, result, now) | Just now <- [good]] ++ seen
        _ -> return seen
    unless (null bad) $ do
        let n = length bad
        errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Old", Just $ show result),("New", Just now)]
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
    withLock lock $
        forM xs $ \x ->
            fst . fromJust <$> Ids.lookup status x

lookupDependencies :: Database -> Key -> IO [Key]
lookupDependencies Database{..} k =
    withLock lock $ do
        intern <- readIORef intern
        let Just i = Intern.lookup k intern
        Just (_, Ready r) <- Ids.lookup status i
        forM (concatMap fromDepends $ depends r) $ \x ->
            fst . fromJust <$> Ids.lookup status x


---------------------------------------------------------------------
-- STORAGE

-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,BinaryEx,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> Result BS.ByteString
toStepResult i = Result (runBuilder $ putEx i) i i [] 0 []

fromStepResult :: Result BS.ByteString -> Step
fromStepResult = getEx . result


withDatabase :: ShakeOptions -> (IO String -> IO ()) -> Map TypeRep (BinaryOp Key) -> (Database -> IO a) -> IO a
withDatabase opts diagnostic witness act = do
    let step = (typeRep (Proxy :: Proxy StepKey), BinaryOp (const mempty) (const stepKey))
    witness <- return $ Map.fromList
        [ (QTypeRep t, BinaryOp (putDatabase putOp) (getDatabase getOp))
        | (t,BinaryOp{..}) <- step : Map.toList witness]
    withStorage opts diagnostic witness $ \status journal -> do
        journal <- return $ \i k v -> journal (QTypeRep $ typeKey k) i (k, Loaded v)

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
        act Database{..}


putDatabase :: (Key -> Builder) -> ((Key, Status) -> Builder)
putDatabase putKey (key, Loaded (Result x1 x2 x3 x4 x5 x6)) =
    putExN (putKey key) <> putExN (putEx x1) <> putEx x2 <> putEx x3 <> putEx x5 <> putExN (putEx x4) <> putEx x6
putDatabase _ (_, x) = errorInternal $ "putWith, Cannot write Status with constructor " ++ statusType x


getDatabase :: (BS.ByteString -> Key) -> BS.ByteString -> (Key, Status)
getDatabase getKey bs
    | (key, bs) <- getExN bs
    , (x1, bs) <- getExN bs
    , (x2, x3, x5, bs) <- binarySplit3 bs
    , (x4, x6) <- getExN bs
    = (getKey key, Loaded (Result x1 x2 x3 (getEx x4) x5 (getEx x6)))

instance BinaryEx Depends where
    putEx (Depends xs) = putExStorableList xs
    getEx = Depends . getExStorableList

instance BinaryEx [Depends] where
    putEx = putExList . map putEx
    getEx = map getEx . getExList

instance BinaryEx Trace where
    putEx (Trace a b c) = putEx b <> putEx c <> putEx a
    getEx x | (b,c,a) <- binarySplit2 x = Trace a b c

instance BinaryEx [Trace] where
    putEx = putExList . map putEx
    getEx = map getEx . getExList
