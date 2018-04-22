{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Core.Run(
    run,
    newCacheIO,
    unsafeExtraThread,
    parallel,
    batch,
    ) where

import Control.Exception
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
import General.Binary
import Development.Shake.Internal.Core.Storage
import Development.Shake.Internal.Core.History
import qualified General.Ids as Ids
import qualified General.Intern as Intern
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Typeable.Extra
import Data.Function
import Data.Either.Extra
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Dynamic
import Data.Maybe
import Data.IORef
import System.Directory
import System.IO.Extra
import System.Time.Extra
import qualified Data.ByteString as BS

import Development.Shake.Classes
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Progress
import Development.Shake.Internal.Value
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Timing
import General.Extra
import General.Concurrent
import General.Cleanup
import Data.Monoid
import Prelude

---------------------------------------------------------------------
-- MAKE

-- | Internal main function (not exported publicly)
run :: ShakeOptions -> Rules () -> IO ()
run opts@ShakeOptions{..} rs = (if shakeLineBuffering then withLineBuffering else id) $ do
    opts@ShakeOptions{..} <- if shakeThreads /= 0 then return opts else do p <- getProcessorCount; return opts{shakeThreads=p}

    start <- offsetTime
    (actions, ruleinfo, userRules) <- runRules opts rs

    outputLocked <- do
        lock <- newLock
        return $ \v msg -> withLock lock $ shakeOutput v msg

    let diagnostic | shakeVerbosity < Diagnostic = const $ return ()
                   | otherwise = \act -> do v <- act; outputLocked Diagnostic $ "% " ++ v
    let output v = outputLocked v . shakeAbbreviationsApply opts
    diagnostic $ return "Starting run"

    except <- newIORef (Nothing :: Maybe (String, ShakeException))
    let raiseError err
            | not shakeStaunch = throwIO err
            | otherwise = do
                let named = shakeAbbreviationsApply opts . shakeExceptionTarget
                atomicModifyIORef except $ \v -> (Just $ fromMaybe (named err, err) v, ())
                -- no need to print exceptions here, they get printed when they are wrapped

    curdir <- getCurrentDirectory
    diagnostic $ return "Starting run 2"
    checkShakeExtra shakeExtra

    after <- newIORef []
    absent <- newIORef []
    withCleanup $ \cleanup -> do
        addCleanup_ cleanup $ do
            when (shakeTimings && shakeVerbosity >= Normal) printTimings
            resetTimings -- so we don't leak memory
        withNumCapabilities shakeThreads $ do
            diagnostic $ return "Starting run 3"
            withDatabase opts diagnostic (Map.map builtinKey ruleinfo) $ \database -> do
                wait <- newBarrier
                let getProgress = do
                        failure <- fmap fst <$> readIORef except
                        stats <- progress database
                        return stats{isFailure=failure}
                tid <- flip forkFinally (const $ signalBarrier wait ()) $
                    shakeProgress getProgress
                addCleanup_ cleanup $ do
                    killThread tid
                    void $ timeout 1 $ waitBarrier wait

                addTiming "Running rules"
                runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                    let s0 = Global database pool cleanup start ruleinfo output opts diagnostic curdir after absent getProgress userRules
                    let s1 = newLocal emptyStack shakeVerbosity
                    forM_ actions $ \act ->
                        addPoolStart pool $ runAction s0 s1 act $ \x -> case x of
                            Left e -> raiseError =<< shakeException s0 ["Top-level action/want"] e
                            Right x -> return x
                maybe (return ()) (throwIO . snd) =<< readIORef except
                assertFinishedDatabase database

                let putWhen lvl msg = when (shakeVerbosity >= lvl) $ output lvl msg

                when (null actions) $
                    putWhen Normal "Warning: No want/action statements, nothing to do"

                when (isJust shakeLint) $ do
                    addTiming "Lint checking"
                    lintCurrentDirectory curdir "After completion"
                    checkValid database (runLint ruleinfo) =<< readIORef absent
                    putWhen Loud "Lint checking succeeded"
                when (shakeReport /= []) $ do
                    addTiming "Profile report"
                    forM_ shakeReport $ \file -> do
                        putWhen Normal $ "Writing report to " ++ file
                        writeProfile file database
                when (shakeLiveFiles /= []) $ do
                    addTiming "Listing live"
                    live <- listLive database
                    let specialIsFileKey t = show (fst $ splitTyConApp t) == "FileQ"
                    let liveFiles = [show k | k <- live, specialIsFileKey $ typeKey k]
                    forM_ shakeLiveFiles $ \file -> do
                        putWhen Normal $ "Writing live list to " ++ file
                        (if file == "-" then putStr else writeFile file) $ unlines liveFiles
            after <- readIORef after
            unless (null after) $ do
                addTiming "Running runAfter"
                sequence_ $ reverse after


listLive :: Database -> IO [Key]
listLive Database{..} = do
    diagnostic $ return "Listing live keys"
    status <- Ids.toList status
    return [k | (_, (k, Ready{})) <- status]



checkShakeExtra :: Map.HashMap TypeRep Dynamic -> IO ()
checkShakeExtra mp = do
    let bad = [(k,t) | (k,v) <- Map.toList mp, let t = dynTypeRep v, t /= k]
    case bad of
        (k,t):xs -> throwIO $ errorStructured "Invalid Map in shakeExtra"
            [("Key",Just $ show k),("Value type",Just $ show t)]
            (if null xs then "" else "Plus " ++ show (length xs) ++ " other keys")
        _ -> return ()


runLint :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> IO (Maybe String)
runLint mp k v = case Map.lookup (typeKey k) mp of
    Nothing -> return Nothing
    Just BuiltinRule{..} -> builtinLint k v


assertFinishedDatabase :: Database -> IO ()
assertFinishedDatabase Database{..} = do
    -- if you have anyone Waiting, and are not exiting with an error, then must have a complex recursion (see #400)
    status <- Ids.toList status
    let bad = [key | (_, (key, Waiting{})) <- status]
    when (bad /= []) $
        throwM $ errorComplexRecursion (map show bad)


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




---------------------------------------------------------------------
-- STORAGE

withDatabase :: ShakeOptions -> (IO String -> IO ()) -> Map.HashMap TypeRep (BinaryOp Key) -> (Database -> IO a) -> IO a
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




---------------------------------------------------------------------
-- RESOURCES


-- | A version of 'Development.Shake.newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newCache' instead.
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO (act :: k -> Action v) = do
    var :: Var (Map.HashMap k (Fence (Either SomeException ([Depends],v)))) <- newVar Map.empty
    return $ \key ->
        join $ liftIO $ modifyVar var $ \mp -> case Map.lookup key mp of
            Just bar -> return $ (,) mp $ do
                res <- liftIO $ testFence bar
                (res,offset) <- case res of
                    Just res -> return (res, 0)
                    Nothing -> do
                        Global{..} <- Action getRO
                        offset <- liftIO offsetTime
                        Action $ captureRAW $ \k -> waitFence bar $ \v ->
                            addPoolResume globalPool $ do offset <- liftIO offset; k $ Right (v,offset)
                case res of
                    Left err -> Action $ throwRAW err
                    Right (deps,v) -> do
                        Action $ modifyRW $ \s -> s{localDepends = deps ++ localDepends s, localDiscount = localDiscount s + offset}
                        return v
            Nothing -> do
                bar <- newFence
                return $ (,) (Map.insert key bar mp) $ do
                    Local{localDepends=pre} <- Action getRW
                    res <- Action $ tryRAW $ fromAction $ act key
                    case res of
                        Left err -> do
                            liftIO $ signalFence bar $ Left err
                            Action $ throwRAW err
                        Right v -> do
                            Local{localDepends=post} <- Action getRW
                            let deps = take (length post - length pre) post
                            liftIO $ signalFence bar $ Right (deps, v)
                            return v


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources.
--   Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot depend on a rule (e.g. 'need') while the extra thread is executing.
--   If the rule blocks (e.g. calls 'withResource') then the extra thread may be used by some other action.
--   Only really suitable for calling 'cmd' / 'command'.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = Action $ do
    Global{..} <- getRO
    stop <- liftIO $ increasePool globalPool
    res <- tryRAW $ fromAction $ blockApply "Within unsafeExtraThread" act
    liftIO stop
    captureRAW $ \continue -> (if isLeft res then addPoolException else addPoolResume) globalPool $ continue res


-- | Execute a list of actions in parallel. In most cases 'need' will be more appropriate to benefit from parallelism.
parallel :: [Action a] -> Action [a]
-- Note: There is no parallel_ unlike sequence_ because there is no stack benefit to doing so
parallel [] = return []
parallel [x] = fmap return x
parallel acts = Action $ do
    global@Global{..} <- getRO
    local <- getRW
    -- number of items still to complete, or Nothing for has completed (by either failure or completion)
    todo :: Var (Maybe Int) <- liftIO $ newVar $ Just $ length acts
    -- a list of refs where the results go
    results :: [IORef (Maybe (Either SomeException (Local, a)))] <- liftIO $ replicateM (length acts) $ newIORef Nothing

    (locals, results) <- captureRAW $ \continue -> do
        let resume = do
                res <- liftIO $ sequence . catMaybes <$> mapM readIORef results
                continue $ fmap unzip res

        liftIO $ forM_ (zip acts results) $ \(act, result) -> do
            let act2 = do
                    whenM (liftIO $ isNothing <$> readVar todo) $
                        fail "parallel, one has already failed"
                    res <- act
                    old <- Action getRW
                    return (old, res)
            addPoolResume globalPool $ runAction global (localClearMutable local) act2 $ \res -> do
                writeIORef result $ Just res
                modifyVar_ todo $ \v -> case v of
                    Nothing -> return Nothing
                    Just i | i == 1 || isLeft res -> do resume; return Nothing
                    Just i -> return $ Just $ i - 1

    modifyRW $ \root -> localMergeMutable root locals
    return results


-- | Batch different outputs into a single 'Action', typically useful when a command has a high
--   startup cost - e.g. @apt-get install foo bar baz@ is a lot cheaper than three separate
--   calls to @apt-get install@. As an example, if we have a standard build rule:
--
-- @
-- \"*.out\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.need' [out '-<.>' \"in\"]
--     'Development.Shake.cmd' "build-multiple" [out '-<.>' \"in\"]
-- @
--
--   Assuming that @build-multiple@ can compile multiple files in a single run,
--   and that the cost of doing so is a lot less than running each individually,
--   we can write:
--
-- @
-- 'batch' 3 (\"*.out\" 'Development.Shake.%>')
--     (\\out -> do 'Development.Shake.need' [out '-<.>' \"in\"]; return out)
--     (\\outs -> 'Development.Shake.cmd' "build-multiple" [out '-<.>' \"in\" | out \<- outs])
-- @
--
--   In constrast to the normal call, we have specified a maximum batch size of 3,
--   an action to run on each output individually (typically all the 'need' dependencies),
--   and an action that runs on multiple files at once. If we were to require lots of
--   @*.out@ files, they would typically be built in batches of 3.
--
--   If Shake ever has nothing else to do it will run batches before they are at the maximum,
--   so you may see much smaller batches, especially at high parallelism settings.
batch
    :: Int   -- ^ Maximum number to run in a single batch, e.g. @3@.
    -> ((a -> Action ()) -> Rules ()) -- ^ Way to match an entry, e.g. @\"*.ext\" '%>'@.
    -> (a -> Action b)  -- ^ Preparation to run individually on each, e.g. using 'need'.
    -> ([b] -> Action ())  -- ^ Combination action to run on all, e.g. using 'cmd'.
    -> Rules ()
batch mx pred one many
    | mx <= 0 = error $ "Can't call batchable with <= 0, you used " ++ show mx
    | mx == 1 = pred $ \a -> do b <- one a; many [b]
    | otherwise = do
        todo :: IORef (Int, [(b, Either SomeException Local -> IO ())]) <- liftIO $ newIORef (0, [])
        pred $ \a -> Action $ do
            b <- fromAction $ one a
            -- optimisation would be to avoid taking the continuation if count >= mx
            -- but it only saves one pool requeue per mx, which is likely to be trivial
            -- and the code becomes a lot more special cases
            global@Global{..} <- getRO
            local <- getRW
            local2 <- captureRAW $ \k -> do
                count <- atomicModifyIORef todo $ \(count, bs) -> ((count+1, (b,k):bs), count+1)
                -- only trigger on the edge so we don't have lots of waiting pool entries
                (if count == mx then addPoolResume else if count == 1 then addPoolBatch else none)
                    globalPool $ go global (localClearMutable local) todo
            modifyRW $ \root -> localMergeMutable root [local2]
    where
        none _ _ = return ()

        go global@Global{..} local todo = do
            (now, count) <- atomicModifyIORef todo $ \(count, bs) ->
                if count <= mx then
                    ((0, []), (bs, 0))
                else
                    let (xs,ys) = splitAt mx bs
                    in ((count - mx, ys), (xs, count - mx))
            (if count >= mx then addPoolResume else if count > 0 then addPoolBatch else none)
                    globalPool $ go global local todo
            unless (null now) $
                runAction global local (do many $ map fst now; Action getRW) $ \x ->
                    forM_ now $ \(_,k) ->
                        (if isLeft x then addPoolException else addPoolResume) globalPool $ k x
