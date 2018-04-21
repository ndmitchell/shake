{-# LANGUAGE RecordWildCards, PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.Core.Database(
    Database, withDatabase, assertFinishedDatabase,
    listDepends, lookupDependencies, lookupStatus,
    BuildKey(..), build,
    progress,
    toReport, checkValid, listLive
    ) where

import Development.Shake.Classes
import General.Binary
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Storage
import Development.Shake.Internal.Options
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Rendezvous
import Development.Shake.Internal.Core.History
import qualified Data.ByteString.Char8 as BS
import General.Extra
import qualified General.Intern as Intern
import General.Intern(Id)

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
import Data.Tuple.Extra
import Data.Either.Extra
import System.Time.Extra
import Data.Monoid
import Prelude

type Map = Map.HashMap


newtype BuildKey = BuildKey
    {buildKey
        :: Stack -- Given the current stack with the key added on
        -> Step -- And the current step
        -> Key -- The key to build
        -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
        -> RunMode -- True if any of the children were dirty
        -> Capture (Either SomeException (Maybe [FilePath], RunResult (Result Value)))
            -- Either an error, or a (the produced files, the result).
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

lookupStatus :: Database -> Key -> IO (Maybe (Either BS.ByteString Value))
lookupStatus Database{..} k = withLock lock $ do
    i <- internKey intern status k
    maybe Nothing (fmap result . getResult . snd) <$> Ids.lookup status i

-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> BuildKey -> (Key -> Value -> BS.ByteString) -> Stack -> [Key] -> Capture (Either SomeException (Seconds,Depends,[Value]))
build pool Database{..} BuildKey{..} identity stack ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern status

        buildMany stack is
            (\v -> case v of Error e -> Just e; _ -> Nothing)
            (\v -> return $ continue $ case v of
                Left e -> Left e
                Right rs -> Right (0, Depends is, map result rs)) $
            \go -> do
                -- only bother doing the stack check if we're actually going to build stuff
                whenJust (checkStack is stack) $ \(badId, badKey) ->
                    -- everything else gets thrown via Left and can be Staunch'd
                    -- recursion in the rules is considered a worse error, so fails immediately
                    errorRuleRecursion (showStack stack ++ [show badKey]) (typeKey badKey) (show badKey)

                time <- offsetTime
                go $ \x -> case x of
                    Left e -> addPoolException pool $ continue $ Left e
                    Right rs -> addPoolResume pool $ do dur <- time; continue $ Right (dur, Depends is, map result rs)
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
                Nothing -> throwM $ errorInternal $ "interned value missing from database, " ++ show i
                Just (k, Missing) -> spawn RunDependenciesChanged stack i k Nothing
                Just (k, Loaded r) -> check stack i k r (depends r)
                Just (k, res) -> return res


        -- | Given a Key and the list of dependencies yet to be checked, check them
        check :: Stack -> Id -> Key -> Result BS.ByteString -> [Depends] -> IO Status {- Ready | Waiting -}
        check stack i k r [] = spawn RunDependenciesSame stack i k $ Just r
        check stack i k r (Depends ds:rest) = do
            let cont v = if isLeft v then spawn RunDependenciesChanged stack i k $ Just r else check stack i k r rest
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
        spawn :: RunMode -> Stack -> Id -> Key -> Maybe (Result BS.ByteString) -> IO Status {- Waiting -}
        spawn mode stack i k r = do
            (w, done) <- newWaiting
            when (mode == RunDependenciesChanged) $ whenJust history $ \history ->
                whenM (hasHistory history k) $ putStrLn $ "CACHE: Should have checked here, " ++ show k
            addPoolStart pool $
                buildKey (addStack i k stack) step k r mode $ \res -> do
                    withLock lock $ do
                        let status = either Error (Ready . runValue . snd) res
                        i #= (k, status)
                        done status
                    case res of
                        Right (produced, RunResult{..}) -> do
                            diagnostic $ return $
                                "result " ++ showBracket k ++ " = "++ showBracket (result runValue) ++
                                " " ++ (if built runValue == changed runValue then "(changed)" else "(unchanged)")
                            unless (runChanged == ChangedNothing) $ do
                                journal i k runValue{result=runStore}
                                unless (runChanged == ChangedStore) $ whenJust history $ \history -> whenJust produced $ \produced -> do
                                    ds <- forM (depends runValue) $ \(Depends is) ->
                                        forM is $ \i -> do
                                            -- if this didn't match then we couldn't have got here
                                            Just (key, Ready value) <- Ids.lookup status i
                                            return (key, identity key $ result value)
                                    addHistory history k ds runStore produced
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
        throwM $ errorComplexRecursion (map show bad)


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
resultsOnly :: Map Id (Key, Status) -> Map Id (Key, Result (Either BS.ByteString Value))
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
    return [maybe (throwImpure $ errorInternal "toReport") f $ Map.lookup i status | i <- order]


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
        throwM $ errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Old", Just $ show result),("New", Just now)]
                                        | (key, result, now) <- bad])
            ""

    bad <- return [(parent,key) | (parent, key) <- missing, isJust $ Intern.lookup key intern]
    unless (null bad) $ do
        let n = length bad
        throwM $ errorStructured
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

withDatabase :: ShakeOptions -> (IO String -> IO ()) -> Map TypeRep (BinaryOp Key) -> (Database -> IO a) -> IO a
withDatabase opts diagnostic owitness act = do
    let step = (typeRep (Proxy :: Proxy StepKey), BinaryOp (const mempty) (const stepKey))
    witness <- return $ Map.fromList
        [ (QTypeRep t, BinaryOp (putDatabase putOp) (getDatabase getOp))
        | (t,BinaryOp{..}) <- step : Map.toList owitness]
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

        history <- case shakeCache opts of
            Nothing -> return Nothing
            Just x -> do
                let wit = binaryOpMap $ Map.fromList $ map (first $ show . QTypeRep) $ Map.toList owitness
                let wit2 = BinaryOp (\k -> putOp wit (show $ QTypeRep $ typeKey k, k)) (snd . getOp wit)
                Just <$> newHistory wit2 x
        act Database{..}


putDatabase :: (Key -> Builder) -> ((Key, Status) -> Builder)
putDatabase putKey (key, Loaded (Result x1 x2 x3 x4 x5 x6)) =
    putExN (putKey key) <> putExN (putEx x1) <> putEx x2 <> putEx x3 <> putEx x5 <> putExN (putEx x4) <> putEx x6
putDatabase _ (_, x) = throwImpure $ errorInternal $ "putWith, Cannot write Status with constructor " ++ statusType x


getDatabase :: (BS.ByteString -> Key) -> BS.ByteString -> (Key, Status)
getDatabase getKey bs
    | (key, bs) <- getExN bs
    , (x1, bs) <- getExN bs
    , (x2, x3, x5, bs) <- binarySplit3 bs
    , (x4, x6) <- getExN bs
    = (getKey key, Loaded (Result x1 x2 x3 (getEx x4) x5 (getEx x6)))
