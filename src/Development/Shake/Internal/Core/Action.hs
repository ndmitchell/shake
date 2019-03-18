{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, ConstraintKinds, TupleSections, ViewPatterns #-}

module Development.Shake.Internal.Core.Action(
    actionOnException, actionFinally, actionCatch, actionRetry,
    getShakeOptions, getProgress, runAfter,
    lintTrackRead, lintTrackWrite, lintTrackAllow,
    getVerbosity, putWhen, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    produces,
    orderOnlyAction,
    newCacheIO,
    unsafeExtraThread,
    parallel,
    batch,
    deprioritize,
    historyDisable,
    traced,
    -- Internal only
    producesUnchecked, producesCheck, lintCurrentDirectory, lintWatch,
    blockApply, unsafeAllowApply, shakeException, lintTrackFinished,
    getCurrentKey, getLocal,
    actionShareList, actionShareRemove
    ) where

import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Typeable
import System.Directory
import System.FilePattern
import System.FilePattern.Directory
import Control.Concurrent.Extra
import Data.Maybe
import Data.Tuple.Extra
import Data.IORef
import Data.List.Extra
import Numeric.Extra
import General.Extra
import qualified Data.HashMap.Strict as Map

import Development.Shake.Classes
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.History.Shared
import General.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.FileName
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Cleanup
import General.Fence


---------------------------------------------------------------------
-- RAW WRAPPERS

-- | Apply a modification, run an action, then run an undo action after.
--   Doesn't actually require exception handling because we don't have the ability to catch exceptions to the user.
actionBracket :: (Local -> (Local, Local -> Local)) -> Action a -> Action a
actionBracket f m = Action $ do
    s <- getRW
    let (s2,undo) = f s
    putRW s2
    res <- fromAction m
    modifyRW undo
    return res


---------------------------------------------------------------------
-- EXCEPTION HANDLING

-- | Turn a normal exception into a ShakeException, giving it a stack and printing it out if in staunch mode.
--   If the exception is already a ShakeException (e.g. it's a child of ours who failed and we are rethrowing)
--   then do nothing with it.
shakeException :: Global -> Stack -> SomeException -> IO ShakeException
shakeException Global{globalOptions=ShakeOptions{..},..} stk e = case fromException e of
    Just (e :: ShakeException) -> return e
    Nothing -> do
        e <- return $ exceptionStack stk e
        when (shakeStaunch && shakeVerbosity >= Quiet) $
            globalOutput Quiet $ show e ++ "Continuing due to staunch mode"
        return e


actionBoom :: Bool -> Action a -> IO b -> Action a
actionBoom runOnSuccess act (void -> clean) = do
    Global{..} <- Action getRO
    key <- liftIO $ register globalCleanup clean
    -- important to mask_ the undo/clean combo so either both happen or neither
    res <- Action $ catchRAW (fromAction act) $ \e -> liftIO (release key) >> throwRAW e
    liftIO $ if runOnSuccess then release key else unprotect key
    return res

-- | If an exception is raised by the 'Action', perform some 'IO' then reraise the exception.
actionOnException :: Action a -> IO b -> Action a
actionOnException = actionBoom False

-- | After an 'Action', perform some 'IO', even if there is an exception.
actionFinally :: Action a -> IO b -> Action a
actionFinally = actionBoom True

-- | If a syncronous exception is raised by the 'Action', perform some handler.
--   Note that there is no guarantee that the handler will run on shutdown (use 'actionFinally' for that),
--   and that 'actionCatch' /cannot/ catch exceptions thrown by dependencies, e.g. raised by 'need'
--   (to do so would allow untracked dependencies on failure conditions).
actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch act hdl = Action $ catchRAW (fromAction act) $ \e ->
    case () of
        _ | not $ isAsyncException e
          , Nothing <- fromException e :: Maybe ShakeException
          , Just e <- fromException e
          -> fromAction $ hdl e
        _ -> throwRAW e


-- | Retry an 'Action' if it throws an exception, at most /n/ times (where /n/ must be positive).
--   If you need to call this function, you should probably try and fix the underlying cause (but you also probably know that).
actionRetry :: Int -> Action a -> Action a
actionRetry i act
    | i <= 0 = fail $ "actionRetry first argument must be positive, got " ++ show i
    | i == 1 = act
    | otherwise = Action $ catchRAW (fromAction act) $ \_ -> fromAction $ actionRetry (i-1) act


---------------------------------------------------------------------
-- QUERIES

-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ globalOptions <$> getRO


-- | Get the current 'Progress' structure, as would be returned by 'shakeProgress'.
getProgress :: Action Progress
getProgress = do
    Global{..} <- Action getRO
    liftIO globalProgress

-- | Specify an action to be run after the database has been closed, if building completes successfully.
runAfter :: IO () -> Action ()
runAfter op = do
    Global{..} <- Action getRO
    liftIO $ atomicModifyIORef globalAfter $ \ops -> (op:ops, ())


---------------------------------------------------------------------
-- VERBOSITY

putWhen :: Verbosity -> String -> Action ()
putWhen v msg = do
    Global{..} <- Action getRO
    verb <- getVerbosity
    when (verb >= v) $
        liftIO $ globalOutput v msg


-- | Write an unimportant message to the output, only shown when 'shakeVerbosity' is higher than normal ('Loud' or above).
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putLoud :: String -> Action ()
putLoud = putWhen Loud

-- | Write a normal priority message to the output, only supressed when 'shakeVerbosity' is 'Quiet' or 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putNormal :: String -> Action ()
putNormal = putWhen Normal

-- | Write an important message to the output, only supressed when 'shakeVerbosity' is 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putQuiet :: String -> Action ()
putQuiet = putWhen Quiet


-- | Get the current verbosity level, originally set by 'shakeVerbosity'. If you
--   want to output information to the console, you are recommended to use
--   'putLoud' \/ 'putNormal' \/ 'putQuiet', which ensures multiple messages are
--   not interleaved. The verbosity can be modified locally by 'withVerbosity'.
getVerbosity :: Action Verbosity
getVerbosity = Action $ localVerbosity <$> getRW


-- | Run an action with a particular verbosity level.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not have any impact on 'Diagnostic' tracing.
withVerbosity :: Verbosity -> Action a -> Action a
withVerbosity new = actionBracket $ \s0 ->
    (s0{localVerbosity=new}, \s -> s{localVerbosity=localVerbosity s0})


-- | Run an action with 'Quiet' verbosity, in particular messages produced by 'traced'
--   (including from 'Development.Shake.cmd' or 'Development.Shake.command') will not be printed to the screen.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not turn off any 'Diagnostic' tracing.
quietly :: Action a -> Action a
quietly = withVerbosity Quiet


---------------------------------------------------------------------
-- BLOCK APPLY

unsafeAllowApply :: Action a -> Action a
unsafeAllowApply  = applyBlockedBy Nothing

blockApply :: String -> Action a -> Action a
blockApply = applyBlockedBy . Just

applyBlockedBy :: Maybe String -> Action a -> Action a
applyBlockedBy reason = actionBracket $ \s0 ->
    (s0{localBlockApply=reason}, \s -> s{localBlockApply=localBlockApply s0})


---------------------------------------------------------------------
-- TRACING

-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'
--   with the name of the executable. The trace list is used for profile reports (see 'shakeReport').
--
--   By default 'traced' prints some useful extra context about what
--   Shake is building, e.g.:
--
-- > # traced message (for myobject.o)
--
--   To suppress the output of 'traced' (for example you want more control
--   over the message using 'putNormal'), use the 'quietly' combinator.
--
--   It is recommended that the string passed to 'traced' is short and that only a small number of unique strings
--   are used (makes profiling work better).
--   The string does not need to make sense on its own, only in conjunction with the target it is building.
traced :: String -> IO a -> Action a
traced msg act = do
    Global{..} <- Action getRO
    Local{localStack} <- Action getRW
    start <- liftIO globalTimestamp
    let key = showTopStack localStack
    putNormal $ "# " ++ msg ++ " (for " ++ key ++ ")"
    res <- liftIO $
        (shakeTrace globalOptions key msg True >> act)
            `finally` shakeTrace globalOptions key msg False
    stop <- liftIO globalTimestamp
    let trace = newTrace msg start stop
    liftIO $ evaluate $ rnf trace
    Action $ modifyRW $ \s -> s{localTraces = mergeTraceTForest trace $ localTraces s}
    return res

mergeTraceTForest :: Trace -> TForest -> TForest
mergeTraceTForest t tf =
  let f (TTree d cs) t2 = TTree d $ if null cs then
                                      [t2] else
                                      map (\t3 -> f t3 t2) cs
      f (TLeaf d) t2 = TTree d [t2]
      n = TLeaf t
      roots = tRoots tf in
    TForest { tRoots = if null roots
                       then [n]
                       else map (\t2 -> f t2 n) roots
            , tracesList = t:(tracesList tf) }

---------------------------------------------------------------------
-- TRACKING

-- | Track that a key has been used/read by the action preceeding it when 'shakeLint' is active.
lintTrackRead :: ShakeValue key => [key] -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) you have already been used by apply, and are on the dependency list
-- 3) someone explicitly gave you permission with trackAllow
-- 4) at the end of the rule, a) you are now on the dependency list, and b) this key itself has no dependencies (is source file)
lintTrackRead ks = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $ do
        l@Local{..} <- Action getRW
        deps <- liftIO $ concatMapM (listDepends globalDatabase) localDepends
        let top = topStack localStack

        let condition1 k = top == Just k
        let condition2 k = k `elem` deps
        let condition3 k = any ($ k) localTrackAllows
        let condition4 = filter (\k -> not $ condition1 k || condition2 k || condition3 k) $ map newKey ks
        unless (null condition4) $
            Action $ putRW l{localTrackUsed = condition4 ++ localTrackUsed}


lintTrackFinished :: Action ()
lintTrackFinished = do
    -- only called when isJust shakeLint
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    liftIO $ do
        deps <- concatMapM (listDepends globalDatabase) localDepends

        -- check 4a
        bad <- return $ localTrackUsed \\ deps
        unless (null bad) $ do
            let n = length bad
            throwM $ errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " used but not depended upon")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check 4b
        bad <- flip filterM localTrackUsed $ \k -> not . null <$> lookupDependencies globalDatabase k
        unless (null bad) $ do
            let n = length bad
            throwM $ errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " depended upon after being used")
                [("Used", Just $ show x) | x <- bad]
                ""


-- | Track that a key has been changed/written by the action preceding it when 'shakeLint' is active.
lintTrackWrite :: ShakeValue key => [key] -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) someone explicitly gave you permission with trackAllow
-- 3) this file is never known to the build system, at the end it is not in the database
lintTrackWrite ks = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $ do
        Local{..} <- Action getRW
        let top = topStack localStack

        let condition1 k = Just k == top
        let condition2 k = any ($ k) localTrackAllows
        let condition3 = filter (\k -> not $ condition1 k || condition2 k) $ map newKey ks
        unless (null condition3) $
            liftIO $ atomicModifyIORef globalTrackAbsent $ \old -> ([(fromMaybe k top, k) | k <- condition3] ++ old, ())


-- | Allow any matching key to violate the tracking rules.
lintTrackAllow :: ShakeValue key => (key -> Bool) -> Action ()
lintTrackAllow (test :: key -> Bool) = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $
        Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        tk = typeRep (Proxy :: Proxy key)
        f k = typeKey k == tk && test (fromKey k)


lintCurrentDirectory :: FilePath -> String -> IO ()
lintCurrentDirectory old msg = do
    now <- getCurrentDirectory
    when (old /= now) $ throwIO $ errorStructured
        "Lint checking error - current directory has changed"
        [("When", Just msg)
        ,("Wanted",Just old)
        ,("Got",Just now)]
        ""

lintWatch :: [FilePattern] -> IO (String -> IO ())
lintWatch [] = return $ const $ return ()
lintWatch pats = do
    let op = getDirectoryFiles "." pats -- cache parsing of the pats
    let record = do xs <- op; forM xs $ \x -> (x,) <$> getFileInfo (fileNameFromString x)
    old <- record
    return $ \msg -> do
        now <- record
        when (old /= now) $ throwIO $ errorStructured
            "Lint checking error - watched files have changed"
            (("When", Just msg) : changes (Map.fromList old) (Map.fromList now))
            ""
    where
        changes old now =
            [("Created", Just x) | x <- Map.keys $ Map.difference now old] ++
            [("Deleted", Just x) | x <- Map.keys $ Map.difference old now] ++
            [("Changed", Just x) | x <- Map.keys $ Map.filter id $ Map.intersectionWith (/=) old now]


listDepends :: Database -> Depends -> IO [Key]
listDepends db (Depends xs) = mapM (fmap (fst . fromJust) . getKeyValueFromId db) xs


lookupDependencies :: Database -> Key -> IO [Depends]
lookupDependencies db k = do
    Just (Ready r) <- getValueFromKey db k
    return $ depends r


-- | This rule should not be cached or recorded in the history because it makes use of untracked dependencies
--   (e.g. files in a system directory or items on the @$PATH@), or is trivial to compute locally.
historyDisable :: Action ()
historyDisable = Action $ modifyRW $ \s -> s{localHistory = False}


-- | This rule builds the following files, in addition to any defined by its target.
--   At the end of the rule these files must have been written.
produces :: [FilePath] -> Action ()
produces xs = Action $ modifyRW $ \s -> s{localProduces = map (True,) (reverse xs) ++ localProduces s}

-- | A version of 'produces' that does not check.
producesUnchecked :: [FilePath] -> Action ()
producesUnchecked xs = Action $ modifyRW $ \s -> s{localProduces = map (False,) (reverse xs) ++ localProduces s}

producesCheck :: Action ()
producesCheck = do
    Local{localProduces} <- Action getRW
    missing <- liftIO $ filterM (notM . doesFileExist_) $ map snd $ filter fst localProduces
    when (missing /= []) $ throwM $ errorStructured
        "Files declared by 'produces' not produced"
        [("File " ++ show i, Just x) | (i,x) <- zipFrom 1 missing]
        ""


-- | Run an action but do not depend on anything the action uses.
--   A more general version of 'orderOnly'.
orderOnlyAction :: Action a -> Action a
orderOnlyAction act = Action $ do
    Local{localDepends=pre} <- getRW
    res <- fromAction act
    modifyRW $ \s -> s{localDepends=pre}
    return res


---------------------------------------------------------------------
-- MORE COMPLEX

-- | A version of 'Development.Shake.newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newCache' instead.
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO (act :: k -> Action v) = do
    var :: Var (Map.HashMap k (Fence IO (Either SomeException ([Depends],v)))) <- newVar Map.empty
    return $ \key ->
        join $ liftIO $ modifyVar var $ \mp -> case Map.lookup key mp of
            Just bar -> return $ (,) mp $ do
                (offset, (deps, v)) <- actionFenceRequeue bar
                Action $ modifyRW $ \s -> addDiscount offset $ s{localDepends = deps ++ localDepends s}
                return v
            Nothing -> do
                bar <- newFence
                return $ (Map.insert key bar mp,) $ do
                    Local{localDepends=pre} <- Action getRW
                    res <- Action $ tryRAW $ fromAction $ act key
                    case res of
                        Left err -> do
                            liftIO $ signalFence bar $ Left err
                            Action $ throwRAW err
                        Right v -> do
                            Local{localDepends=post} <- Action getRW
                            let deps = dropEnd (length pre) post
                            liftIO $ signalFence bar $ Right (deps, v)
                            return v


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources.
--   Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot depend on a rule (e.g. 'need') while the extra thread is executing.
--   If the rule blocks (e.g. calls 'withResource') then the extra thread may be used by some other action.
--   Only really suitable for calling 'cmd' / 'command'.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = do
    Global{..} <- Action getRO
    stop <- liftIO $ increasePool globalPool
    res <- Action $ tryRAW $ fromAction $ blockApply "Within unsafeExtraThread" act
    liftIO stop
    -- we start a new thread, giving up ours, to ensure the thread count goes down
    (wait, res) <- actionAlwaysRequeue res
    Action $ modifyRW $ addDiscount wait
    return res


-- | Execute a list of actions in parallel. In most cases 'need' will be more appropriate to benefit from parallelism.
parallel :: [Action a] -> Action [a]
-- Note: There is no parallel_ unlike sequence_ because there is no stack benefit to doing so
parallel [] = return []
parallel [x] = return <$> x
parallel acts = do
    Global{..} <- Action getRO

    done <- liftIO $ newIORef False
    waits <- forM acts $ \act ->
        addPoolWait PoolResume $ do
            whenM (liftIO $ readIORef done) $
                fail "parallel, one has already failed"
            Action $ modifyRW localClearMutable
            res <- act
            old <- Action getRW
            return (old, res)
    (wait, res) <- actionFenceSteal =<< liftIO (exceptFence waits)
    liftIO $ atomicWriteIORef done True
    let (waits, locals, results) = unzip3 $ map (\(a,(b,c)) -> (a,b,c)) res
    Action $ modifyRW $ \root -> addDiscount (wait - sum waits) $ localMergeMutable root locals
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
    :: Int   -- ^ Maximum number to run in a single batch, e.g. @3@, must be positive.
    -> ((a -> Action ()) -> Rules ()) -- ^ Way to match an entry, e.g. @\"*.ext\" '%>'@.
    -> (a -> Action b)  -- ^ Preparation to run individually on each, e.g. using 'need'.
    -> ([b] -> Action ())  -- ^ Combination action to run on all, e.g. using 'cmd'.
    -> Rules ()
batch mx pred one many
    | mx <= 0 = error $ "Can't call batchable with <= 0, you used " ++ show mx
    | mx == 1 = pred $ \a -> do b <- one a; many [b]
    | otherwise = do
        todo :: IORef (Int, [(b, Local, Fence IO (Either SomeException Local))]) <- liftIO $ newIORef (0, [])
        pred $ \a -> do
            b <- one a
            fence <- liftIO newFence
            -- add one to the batch
            local <- Action getRW
            count <- liftIO $ atomicModifyIORef todo $ \(count, bs) -> let i = count+1 in ((i, (b,local,fence):bs), i)
            requeue todo (==) count
            (wait, local2) <- actionFenceRequeue fence
            Action $ modifyRW $ \root -> addDiscount wait $ localMergeMutable root [local2]
    where
        -- When changing by one, only trigger on (==) so we don't have lots of waiting pool entries
        -- When changing by many, trigger on (>=) because we don't hit all edges
        requeue todo trigger count
            | count `trigger` mx = addPoolWait_ PoolResume $ go todo
            | count `trigger` 1  = addPoolWait_ PoolBatch  $ go todo
            | otherwise = return ()

        go todo = do
            -- delete at most mx from the batch
            (now, count) <- liftIO $ atomicModifyIORef todo $ \(count, bs) ->
                let (now,later) = splitAt mx bs
                    count2 = if count > mx then count - mx else 0
                in ((count2, later), (now, count2))
            requeue todo (>=) count

            unless (null now) $ do
                res <- Action $ tryRAW $ do
                    -- make sure we are using one of the local's that we are computing
                    -- we things like stack, blockApply etc. work as expected
                    modifyRW $ const $ localClearMutable $ snd3 $ head now
                    fromAction $ many $ map fst3 now
                    res <- getRW
                    return res{localDiscount = localDiscount res / intToDouble (length now)} -- divide the batch fairly
                liftIO $ mapM_ (flip signalFence res . thd3) now


-- | Given a running task, deprioritize so it only continues after all other pending tasks,
--   and all deprioritized tasks with a higher priority. Note that due to parallelism there is no guarantee
--   that all actions of a higher priority will have /completed/ before the action resumes.
--   Only useful if the results are being interactively reported or consumed.
deprioritize :: Double -> Action ()
deprioritize x = do
    (wait, _) <- actionAlwaysRequeuePriority (PoolDeprioritize $ negate x) $ return ()
    Action $ modifyRW $ addDiscount wait


getCurrentKey :: Action (Maybe Key)
getCurrentKey = Action $ topStack . localStack <$> getRW

getLocal :: Action Local
getLocal = Action getRW

-- | Hooked up to --share-remove
actionShareRemove :: [String] -> Action ()
actionShareRemove substrs = do
    Global{..} <- Action getRO
    case globalShared of
        Nothing -> throwM $ errorInternal "actionShareRemove with no shared"
        Just x -> liftIO $ removeShared x $ \k -> any (`isInfixOf` show k) substrs

-- | Hooked up to --share-list
actionShareList :: Action ()
actionShareList = do
    Global{..} <- Action getRO
    case globalShared of
        Nothing -> throwM $ errorInternal "actionShareList with no shared"
        Just x -> liftIO $ listShared x
