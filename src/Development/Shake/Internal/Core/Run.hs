{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Core.Run(
    run,
    Action, actionOnException, actionFinally, apply, apply1, traced,
    getShakeOptions, getProgress,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    Resource, newResourceIO, withResource, newThrottleIO,
    newCacheIO,
    unsafeExtraThread, unsafeAllowApply,
    parallel,
    orderOnlyAction,
    batch,
    -- Internal stuff
    runAfter
    ) where

import Control.Exception.Extra
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
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
import Numeric.Extra
import qualified Data.ByteString as BS

import Development.Shake.Classes
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Resource
import Development.Shake.Internal.Value
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Timing
import General.Extra
import General.Concurrent
import General.Cleanup
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
        _ <- addCleanup cleanup $ do
            when shakeTimings printTimings
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
                _ <- addCleanup cleanup $ do
                    killThread tid
                    void $ timeout 1000000 $ waitBarrier wait

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
                    absent <- readIORef absent
                    checkValid database (runLint ruleinfo) absent
                    putWhen Loud "Lint checking succeeded"
                when (shakeReport /= []) $ do
                    addTiming "Profile report"
                    report <- toReport database
                    forM_ shakeReport $ \file -> do
                        putWhen Normal $ "Writing report to " ++ file
                        writeProfile file report
                when (shakeLiveFiles /= []) $ do
                    addTiming "Listing live"
                    live <- listLive database
                    let specialIsFileKey t = show (fst $ splitTyConApp t) == "FileQ"
                    let liveFiles = [show k | k <- live, specialIsFileKey $ typeKey k]
                    forM_ shakeLiveFiles $ \file -> do
                        putWhen Normal $ "Writing live list to " ++ file
                        (if file == "-" then putStr else writeFile file) $ unlines liveFiles
            sequence_ . reverse =<< readIORef after


checkShakeExtra :: Map.HashMap TypeRep Dynamic -> IO ()
checkShakeExtra mp = do
    let bad = [(k,t) | (k,v) <- Map.toList mp, let t = dynTypeRep v, t /= k]
    case bad of
        (k,t):xs -> errorStructured "Invalid Map in shakeExtra"
            [("Key",Just $ show k),("Value type",Just $ show t)]
            (if null xs then "" else "Plus " ++ show (length xs) ++ " other keys")
        _ -> return ()


lintCurrentDirectory :: FilePath -> String -> IO ()
lintCurrentDirectory old msg = do
    now <- getCurrentDirectory
    when (old /= now) $ errorStructured
        "Lint checking error - current directory has changed"
        [("When", Just msg)
        ,("Wanted",Just old)
        ,("Got",Just now)]
        ""


withLineBuffering :: IO a -> IO a
withLineBuffering act = do
    -- instead of withBuffering avoid two finally handlers and stack depth
    out <- hGetBuffering stdout
    err <- hGetBuffering stderr
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    act `finally` do
        hSetBuffering stdout out
        hSetBuffering stderr err


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'addUserRule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: (RuleResult key ~ value, ShakeValue key, ShakeValue value) => [key] -> Action [value]
-- Don't short-circuit [] as we still want error messages
apply (ks :: [key]) = withResultType $ \(p :: Maybe (Action [value])) -> do
    -- this is the only place a user can inject a key into our world, so check they aren't throwing
    -- in unevaluated bottoms
    liftIO $ mapM_ (evaluate . rnf) ks

    let tk = typeRep (Proxy :: Proxy key)
        tv = typeRep (Proxy :: Proxy value)
    Global{..} <- Action getRO
    block <- Action $ getsRW localBlockApply
    whenJust block $ liftIO . errorNoApply tk (show <$> listToMaybe ks)
    case Map.lookup tk globalRules of
        Nothing -> liftIO $ errorNoRuleToBuildType tk (show <$> listToMaybe ks) (Just tv)
        Just BuiltinRule{builtinResult=tv2} | tv /= tv2 -> errorInternal $ "result type does not match, " ++ show tv ++ " vs " ++ show tv2 
        _ -> fmap (map fromValue) $ applyKeyValue $ map newKey ks


applyKeyValue :: [Key] -> Action [Value]
applyKeyValue [] = return []
applyKeyValue ks = do
    global@Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    (dur, dep, vs) <- Action $ captureRAW $ build globalPool globalDatabase (BuildKey $ runKey global) stack ks
    Action $ modifyRW $ \s -> s{localDiscount=localDiscount s + dur, localDepends=dep : localDepends s}
    return vs


runKey :: Global -> Stack -> Step -> Key -> Maybe (Result BS.ByteString) -> Bool -> Capture (Either SomeException (Bool, BS.ByteString, Result Value))
runKey global@Global{globalOptions=ShakeOptions{..},..} stack step k r dirtyChildren continue = do
    let tk = typeKey k
    BuiltinRule{..} <- case Map.lookup tk globalRules of
        Nothing -> errorNoRuleToBuildType tk (Just $ show k) Nothing
        Just r -> return r

    let s = newLocal stack shakeVerbosity
    time <- offsetTime
    runAction global s (do
        res <- builtinRun k (fmap result r) dirtyChildren
        liftIO $ evaluate $ rnf res
        when (Just LintFSATrace == shakeLint) trackCheckUsed
        Action $ fmap ((,) res) getRW) $ \x -> case x of
            Left e -> do
                e <- if isNothing shakeLint then return e else handle return $
                    do lintCurrentDirectory globalCurDir $ "Running " ++ show k; return e
                continue . Left . toException =<< shakeException global (showStack stack) e
            Right (RunResult{..}, Local{..})
                | runChanged == ChangedNothing || runChanged == ChangedStore, Just r <- r ->
                    continue $ Right (runChanged == ChangedStore, runStore, r{result = runValue})
                | otherwise -> do
                    dur <- time
                    let c | Just r <- r, runChanged == ChangedRecomputeSame = changed r
                          | otherwise = step
                    continue $ Right $ (,,) True runStore Result
                        {result = runValue
                        ,changed = c
                        ,built = step
                        ,depends = reverse localDepends
                        ,execution = doubleToFloat $ dur - localDiscount
                        ,traces = reverse localTraces}


runLint :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> IO (Maybe String)
runLint mp k v = case Map.lookup (typeKey k) mp of
    Nothing -> return Nothing
    Just BuiltinRule{..} -> builtinLint k v


-- | Turn a normal exception into a ShakeException, giving it a stack and printing it out if in staunch mode.
--   If the exception is already a ShakeException (e.g. it's a child of ours who failed and we are rethrowing)
--   then do nothing with it.
shakeException :: Global -> [String] -> SomeException -> IO ShakeException
shakeException Global{globalOptions=ShakeOptions{..},..} stk e@(SomeException inner) = case cast inner of
    Just e@ShakeException{} -> return e
    Nothing -> do
        e <- return $ ShakeException (last $ "Unknown call stack" : stk) stk e
        when (shakeStaunch && shakeVerbosity >= Quiet) $
            globalOutput Quiet $ show e ++ "Continuing due to staunch mode"
        return e


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: (RuleResult key ~ value, ShakeValue key, ShakeValue value) => key -> Action value
apply1 = fmap head . apply . return


---------------------------------------------------------------------
-- RESOURCES

-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
--   You cannot depend on a rule (e.g. 'need') while a resource is held.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = do
    Global{..} <- Action getRO
    liftIO $ globalDiagnostic $ return $ show r ++ " waiting to acquire " ++ show i
    offset <- liftIO offsetTime
    Action $ captureRAW $ \continue -> acquireResource r globalPool i $ continue $ Right ()
    res <- Action $ tryRAW $ fromAction $ blockApply ("Within withResource using " ++ show r) $ do
        offset <- liftIO offset
        liftIO $ globalDiagnostic $ return $ show r ++ " acquired " ++ show i ++ " in " ++ showDuration offset
        Action $ modifyRW $ \s -> s{localDiscount = localDiscount s + offset}
        act
    liftIO $ releaseResource r globalPool i
    liftIO $ globalDiagnostic $ return $ show r ++ " released " ++ show i
    Action $ either throwRAW return res


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
                        pool <- Action $ getsRO globalPool
                        offset <- liftIO offsetTime
                        Action $ captureRAW $ \k -> waitFence bar $ \v ->
                            addPoolResume pool $ do offset <- liftIO offset; k $ Right (v,offset)
                case res of
                    Left err -> Action $ throwRAW err
                    Right (deps,v) -> do
                        Action $ modifyRW $ \s -> s{localDepends = deps ++ localDepends s, localDiscount = localDiscount s + offset}
                        return v
            Nothing -> do
                bar <- newFence
                return $ (,) (Map.insert key bar mp) $ do
                    pre <- Action $ getsRW localDepends
                    res <- Action $ tryRAW $ fromAction $ act key
                    case res of
                        Left err -> do
                            liftIO $ signalFence bar $ Left err
                            Action $ throwRAW err
                        Right v -> do
                            post <- Action $ getsRW localDepends
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


-- | Run an action but do not depend on anything the action uses.
--   A more general version of 'orderOnly'.
orderOnlyAction :: Action a -> Action a
orderOnlyAction act = Action $ do
    pre <- getsRW localDepends
    res <- fromAction act
    modifyRW $ \s -> s{localDepends=pre}
    return res


batch
    :: Int
    -> ((a -> Action ()) -> Rules ())
    -> (a -> Action b)
    -> ([b] -> Action ())
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
