{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, DeriveFunctor, ConstraintKinds, Rank2Types #-}

module Development.Shake.Core2(
    Action(..), runAction, Global(..), Local,
    SRules(..), BuiltinRule(..), BuiltinResult(..), Result(..),
    UserRule(..), RuleSet(..), userRuleMatch, userRule, ruleValue,
    run', apply, applied, blockApply, unsafeAllowApply, withResource, newCacheIO,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    traced, trackUse, trackChange, trackAllow, orderOnlyAction
    ) where

import Control.Exception.Extra
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Typeable
import Data.Dynamic
import Data.Function
import Data.Either.Extra
import Numeric.Extra
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.IORef
import System.Directory
import System.IO.Extra
import System.Time.Extra
import Data.Monoid
import System.IO.Unsafe

import Development.Shake.Classes
import Development.Shake.Pool
import Development.Shake.Database
import Development.Shake.Monad
import Development.Shake.Resource
import Development.Shake.Value
import Development.Shake.Profile
import Development.Shake.Types
import Development.Shake.Errors
import General.Timing
import General.Concurrent
import General.Cleanup
import General.String
import Prelude


-- | A 'Match' data type, representing user-defined rules associated with a particular type.
--   As an example '?>' and '*>' will add entries to the 'Match' data type.
--
--   /Semantics/
--
-- > priority p1 (priority p2 x) == priority p1 x
-- > priority p (x `ordered` y) = priority p x `ordered` priority p y
-- > priority p (x `unordered` y) = priority p x `unordered` priority p y
-- > ordered is associative
-- > unordered is associative and commutative
-- > alternative does not obey priorities, until picking the best one
data UserRule a
    = UserRule a
    | Unordered [UserRule a] -- ^ Added to the state with @'addUserRule' :: Typeable a => a -> Rules ()@.
    | Priority Double (UserRule a) -- ^ Rules defined under 'priority'.
    | Alternative (UserRule a) -- ^ matched in order.
      deriving (Eq,Show,Functor)

-- | Rules might be able to be optimised in some cases
userRuleMatch :: UserRule (Maybe a) -> [[a]]
userRuleMatch = map snd . reverse . groupSort . f
    where
        f :: UserRule (Maybe a) -> [(Double,a)]
        f (UserRule x) = maybe [] (\x -> [(1,x)]) x
        f (Unordered xs) = concatMap f xs
        f (Priority d x) = map (first $ const d) $ f x
        f (Alternative x) = take 1 $ f x

userRule :: (Typeable k, Show k) => UserRule a -> (a -> Maybe b) -> k -> IO b
userRule u f k = case userRuleMatch (fmap f u) of
    [r]:_ -> return r
    rs:_  -> errorMultipleRulesMatch (typeOf k) (Just $ show k) (length rs)
    []    -> errorMultipleRulesMatch (typeOf k) (Just $ show k) 0

combineRules :: UserRule a -> UserRule a -> UserRule a
combineRules x (Unordered xs) = Unordered (x:xs)
combineRules (Unordered xs) x = Unordered (xs++[x])
combineRules x y = Unordered [x,y]

data RuleSet = forall a. (Typeable a) => ARule { fromARule :: UserRule a }

data SRules m = SRules
    {actions :: [m ()]
    ,rules :: Map.HashMap TypeRep (BuiltinRule m)
    ,userrules :: Map.HashMap TypeRep RuleSet
    }

instance Typeable m => Monoid (SRules m) where
    mempty = SRules [] Map.empty Map.empty
    mappend (SRules x1 x2 x3) (SRules y1 y2 y3) = SRules (x1++y1) (Map.unionWithKey f x2 y2) (Map.unionWithKey g x3 y3)
      where f k _ _ = unsafePerformIO $ errorMultipleRulesMatch k Nothing 2
            g k (ARule x) (ARule y)
                  | Just y' <- cast y = ARule (combineRules x y')
                  | otherwise = unsafePerformIO $ errorIncompatibleRules k (typeOf $ ruleValue x) (typeOf $ ruleValue y)

ruleValue :: UserRule a -> a
ruleValue _ = err "ruleValue"

data BuiltinRule m = BuiltinRule
    { execute :: Key -- ^ Key that you want to build.
              -> Maybe Result -- ^ the previous result in the database, if any
              -> Bool -- ^ 'True' if any dependency has changed, or if Shake has no memory of this rule.
              -> m BuiltinResult -- ^ result of executing the rule
    }

data BuiltinResult = BuiltinResult
    { resultStoreB :: Value -- ^ the result associated with the Key
    , resultValueB :: Dynamic -- ^ dynamic return value limited to lifetime of the program
    , dependsB :: Maybe Depends -- ^ dependencies, or Nothing to use inferred dependencies
    , changedB :: Bool
    }

---------------------------------------------------------------------
-- MAKE

-- global constants of Action
data Global = Global
    {globalDatabase :: Database
    ,globalPool :: Pool
    ,globalCleanup :: Cleanup
    ,globalTimestamp :: IO Seconds
    ,globalRules :: Map.HashMap TypeRep (BuiltinRule Action)
    ,globalUserRules :: Map.HashMap TypeRep RuleSet
    ,globalOutput :: Verbosity -> String -> IO ()
    ,globalOptions  :: ShakeOptions
    ,globalDiagnostic :: String -> IO ()
    ,globalLint :: String -> IO ()
    ,globalAfter :: IORef [IO ()]
    ,globalTrackAbsent :: IORef [(Key, Key)] -- in rule fst, snd must be absent
    ,globalProgress :: IO Progress
    ,globalForwards :: IORef (Map.HashMap Key (Action ()))
    }

-- local variables of Action
data Local = Local
    -- constants
    {localStack :: Stack
    -- stack scoped local variables
    ,localVerbosity :: Verbosity
    ,localBlockApply ::  Maybe String -- reason to block apply, or Nothing to allow
    -- mutable local variables
    ,localDepends :: Depends -- built up in reverse
    ,localDiscount :: !Seconds
    ,localTraces :: [Trace] -- in reverse
    ,localTrackAllows :: [Key -> Bool]
    ,localTrackUsed :: [Key]
    }

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'rule' and 'action'. The 'Action' monad tracks the dependencies of a 'Rule'.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Whether to do basic lint checks
basicLint :: Lint -> Bool
basicLint = (/= LintNothing)

-- | Internal main function (not exported publicly)
run' :: ShakeOptions -> IO Seconds -> SRules Action -> IO ()
run' opts@ShakeOptions{..} start rs = do
    outputLocked <- do
        lock <- newLock
        return $ \v msg -> withLock lock $ shakeOutput v msg

    let diagnostic = if shakeVerbosity >= Diagnostic then outputLocked Diagnostic . ("% "++) else const $ return ()
    let output v = outputLocked v . abbreviate shakeAbbreviations
    diagnostic "Starting run"

    except <- newIORef (Nothing :: Maybe (String, ShakeException))
    let raiseError err
            | not shakeStaunch = throwIO err
            | otherwise = do
                let named = abbreviate shakeAbbreviations . shakeExceptionTarget
                atomicModifyIORef except $ \v -> (Just $ fromMaybe (named err, err) v, ())
                -- no need to print exceptions here, they get printed when they are wrapped

    lint <- if shakeLint == LintNothing then return $ const $ return () else do
        dir <- getCurrentDirectory
        return $ \msg -> do
            diagnostic msg
            now <- getCurrentDirectory
            when (dir /= now) $ errorStructured
                "Lint checking error - current directory has changed"
                [("When", Just msg)
                ,("Wanted",Just dir)
                ,("Got",Just now)]
                ""
    diagnostic "Starting run 2"

    after <- newIORef []
    absent <- newIORef []
    forwards <- newIORef Map.empty
    withCleanup $ \cleanup -> do
        _ <- addCleanup cleanup $ do
            when shakeTimings printTimings
            resetTimings -- so we don't leak memory
        withNumCapabilities shakeThreads $ do
            diagnostic "Starting run 3"
            withDatabase opts diagnostic $ \database -> do
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
                    let s0 = Global database pool cleanup start (rules rs) (userrules rs) output opts diagnostic lint after absent getProgress forwards
                    let s1 = Local emptyStack shakeVerbosity Nothing mempty 0 [] [] []
                    forM_ (actions rs) $ \act ->
                        addPool pool $ runAction s0 s1 act $ \x -> case x of
                            Left e -> raiseError =<< shakeException s0 (return ["Top-level action/want"]) e
                            Right x -> return x
                maybe (return ()) (throwIO . snd) =<< readIORef except
                assertFinishedDatabase database

                when (null $ actions rs) $
                    when (shakeVerbosity >= Normal) $ output Normal "Warning: No want/action statements, nothing to do"

                when (basicLint shakeLint) $ do
                    addTiming "Lint checking"
                    absent' <- readIORef absent
                    checkValid database absent' $ \ks -> do
                        bad <- newIORef []
                        runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                            let opts2 = opts{shakeRunCommands=RunMinimal}
                            let s0 = Global database pool cleanup start (rules rs) (userrules rs) output opts2 diagnostic lint after absent getProgress forwards
                            forM_ ks $ \(i,(key,v)) -> case v of
                                Ready ro -> do
                                    let reply = undefined
--                                         reply (Error e) = raiseError =<< shakeException s0 (return ["Lint-checking"]) e
--                                         reply (Ready r) = do
--                                             let now = built r == changed r
--                                             diagnostic $ "Checking if " ++ show key ++ " is " ++ show (result ro) ++ ", " ++ if now then "passed" else "FAILED"
--                                             whenJust now $ \now -> modifyIORef' bad ((key, result ro, now):)
                                    runKey_
                                        s0 i key (Just $ resultStore ro) False
                                        emptyStack
                                        (incStep $ built $ resultStore ro)
                                        reply
                        maybe (return ()) (throwIO . snd) =<< readIORef except
                        readIORef bad
                    when (shakeVerbosity >= Loud) $ output Loud "Lint checking succeeded"
                when (shakeReport /= []) $ do
                    addTiming "Profile report"
                    report <- toReport database
                    forM_ shakeReport $ \file -> do
                        when (shakeVerbosity >= Normal) $
                            output Normal $ "Writing report to " ++ file
                        writeProfile file report
                when (shakeLiveFiles /= []) $ do
                    addTiming "Listing live"
                    -- TODO: actual pruning
--                     live <- listLive database
--                     let liveFiles = [show k | k <- live, specialIsFileKey $ typeKey k]
--                     forM_ shakeLiveFiles $ \file -> do
--                         when (shakeVerbosity >= Normal) $
--                             output Normal $ "Writing live list to " ++ file
--                         (if file == "-" then putStr else writeFile file) $ unlines liveFiles
            sequence_ . reverse =<< readIORef after

runAction :: Global -> Local -> Action a -> Capture (Either SomeException a)
runAction g l (Action x) = runRAW g l x

-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'rule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: (ShakeValue key, ShakeValue value) => [key] -> Action [value]
apply = applyForall

-- | Return the values associated with an already-executed rule, throwing an error if the
--   rule would need to be re-run.
--   This function requires that appropriate rules have been added with 'rule'.
--   All @key@ values passed to 'applied' become dependencies of the 'Action'.
applied :: (ShakeValue key, ShakeValue value) => [key] -> Action [value]
applied ks = blockApply "'applied' key" (applyForall ks)

-- We don't want the forall in the Haddock docs
-- Don't short-circuit [] as we still want error messages
applyForall :: forall key value . (ShakeValue key, ShakeValue value) => [key] -> Action [value]
applyForall ks = do
    let tk = typeOf (err "apply key" :: key)
        tv = typeOf (err "apply type" :: value)
    Global{..} <- Action getRO
    -- this duplicates the check in runKey, but we can give better error messages here
    case Map.lookup tk globalRules of
        Nothing -> liftIO $ errorNoRuleToBuildType tk (show <$> listToMaybe ks) (Just tv)
        _ -> return ()
    vs <- applyKeyValue (map newKey ks)
    let f k (resultValue -> v) = case fromDynamic v of
            Just v -> return v
            Nothing -> liftIO $ errorRuleTypeMismatch tk (Just $ show k) (dynTypeRep v) tv
    zipWithM f ks vs

applyKeyValue :: [Key] -> Action [LiveResult]
applyKeyValue [] = return []
applyKeyValue ks = do
    global@Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    block <- Action $ getsRW localBlockApply
    (dur, dep, vs) <- Action $ captureRAW $ build globalPool globalDatabase (Ops (runKey_ global)) stack block ks
    Action $ modifyRW $ \s -> s{localDiscount=localDiscount s + dur, localDepends=dep <> localDepends s}
    return vs

runKey_ :: Global -> Id -> Key -> Maybe Result -> Bool -> Stack -> Step -> Capture Status
runKey_ global@Global{..} i k r deps stack step continue = do
    time <- offsetTime
    let s = Local stack (shakeVerbosity globalOptions) Nothing mempty 0 [] [] []
    let top = showTopStack stack
    runAction global s (do
        let tk = typeKey k
        case Map.lookup tk globalRules of
            Nothing -> liftIO $ errorNoRuleToBuildType tk (Just $ show k) Nothing
            Just BuiltinRule{..} -> do
                liftIO $ evaluate $ rnf k
                liftIO $ globalLint $ "before building " ++ top
                putWhen Chatty $ "# " ++ show k
                BuiltinResult{..} <- execute k r deps
                when (LintFSATrace == shakeLint globalOptions) trackCheckUsed
                liftIO $ globalLint $ "after building " ++ top
                dur <- liftIO time
                Local{..} <- Action $ getRW
                let ans = LiveResult
                          { resultValue = resultValueB
                          , resultStore = Result
                            { result = resultStoreB
                            , depends = fromMaybe (finalizeDepends localDepends) dependsB
                            , changed = if changedB then step else maybe step changed r
                            , built = step
                            , execution = doubleToFloat $ dur - localDiscount
                            }
                          , traces = reverse localTraces
                          }
                liftIO $ evaluate $ rnf ans
                return ans
                ) $ \x -> case x of
                    Left e -> continue . Error . toException =<< shakeException global (showStack globalDatabase stack) e
                    Right r -> continue $ Ready r

-- | Turn a normal exception into a ShakeException, giving it a stack and printing it out if in staunch mode.
--   If the exception is already a ShakeException (e.g. it's a child of ours who failed and we are rethrowing)
--   then do nothing with it.
shakeException :: MonadIO m => Global -> m [String] -> SomeException -> m ShakeException
shakeException Global{globalOptions=ShakeOptions{..},..} stk e@(SomeException inner) = case cast inner of
    Just e@ShakeException{} -> return e
    Nothing -> do
        stk <- stk
        e <- return $ ShakeException (last $ "Unknown call stack" : stk) stk e
        when (shakeStaunch && shakeVerbosity >= Quiet) $
            liftIO . globalOutput Quiet $ show e ++ "Continuing due to staunch mode"
        return e

-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'.
--   The trace list is used for profile reports (see 'shakeReport').
--
--   By default 'traced' prints some useful extra context about what
--   Shake is building, e.g.:
--
-- > # traced message (for myobject.o)
--
--   To suppress the output of 'traced' (for example you want more control
--   over the message using 'putNormal'), use the 'quietly' combinator.
traced :: String -> IO a -> Action a
traced s a = traced' s (liftIO a)

traced' :: String -> Action a -> Action a
traced' msg act = do
    Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    start <- liftIO globalTimestamp
    putNormal $ "# " ++ msg ++ " (for " ++ showTopStack stack ++ ")"
    res <- act
    stop <- liftIO globalTimestamp
    Action $ modifyRW $ \s -> s{localTraces = Trace (pack msg) (doubleToFloat start) (doubleToFloat stop) : localTraces s}
    return res

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
getVerbosity = Action $ getsRW localVerbosity

-- | Run an action with a particular verbosity level.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not have any impact on 'Diagnostic' tracing.
withVerbosity :: Verbosity -> Action a -> Action a
withVerbosity new = Action . unmodifyRW f . fromAction
    where f s0 = (s0{localVerbosity=new}, \s -> s{localVerbosity=localVerbosity s0})

-- | Run an action with 'Quiet' verbosity, in particular messages produced by 'traced'
--   (including from 'Development.Shake.cmd' or 'Development.Shake.command') will not be printed to the screen.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not turn off any 'Diagnostic' tracing.
quietly :: Action a -> Action a
quietly = withVerbosity Quiet

---------------------------------------------------------------------
-- TRACKING

-- | Track that a key has been used by the action preceeding it.
trackUse :: ShakeValue key => key -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) you have already been used by apply, and are on the dependency list
-- 3) someone explicitly gave you permission with trackAllow
-- 4) at the end of the rule, a) you are now on the dependency list, and b) this key itself has no dependencies (is source file)
trackUse key = do
    Global{..} <- Action getRO
    l@Local{..} <- Action getRW
    when (basicLint $ shakeLint globalOptions) $ do
        let k = newKey key
        deps <- liftIO $ listDepends globalDatabase localDepends
        let top = topStack localStack
        if top == Just k then
            return () -- condition 1
        else if k `elem` deps then
            return () -- condition 2
        else if any ($ k) localTrackAllows then
            return () -- condition 3
        else
            Action $ putRW l{localTrackUsed = k : localTrackUsed} -- condition 4


trackCheckUsed :: Action ()
trackCheckUsed = do
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    when (basicLint $ shakeLint globalOptions) $ liftIO $ do
        deps <- listDepends globalDatabase localDepends

        -- check 3a
        bad <- return $ localTrackUsed \\ deps
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " used but not depended upon")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check 3b
        bad <- flip filterM localTrackUsed $ \k -> (not . null) <$> lookupDependencies globalDatabase k
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " depended upon after being used")
                [("Used", Just $ show x) | x <- bad]
                ""


-- | Track that a key has been changed by the action preceeding it.
trackChange :: ShakeValue key => key -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) someone explicitly gave you permission with trackAllow
-- 3) this file is never known to the build system, at the end it is not in the database
trackChange key = do
    let k = newKey key
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    when (basicLint $ shakeLint globalOptions) $ liftIO $ do
        let top = topStack localStack
        if top == Just k then
            return () -- condition 1
         else if any ($ k) localTrackAllows then
            return () -- condition 2
         else
            -- condition 3
            atomicModifyIORef globalTrackAbsent $ \ks -> ((fromMaybe k top, k):ks, ())


-- | Allow any matching key to violate the tracking rules.
trackAllow :: ShakeValue key => (key -> Bool) -> Action ()
trackAllow = trackAllowForall

-- We don't want the forall in the Haddock docs
trackAllowForall :: forall key . ShakeValue key => (key -> Bool) -> Action ()
trackAllowForall test = do
    Global{..} <- Action getRO
    when (basicLint $ shakeLint globalOptions) $ Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        tk = typeOf (err "trackAllow key" :: key)
        f k = typeKey k == tk && fmap test (fromKey k) == Just True

---------------------------------------------------------------------
-- RESOURCES

applyBlockedBy :: Maybe String -> Action a -> Action a
applyBlockedBy reason = Action . unmodifyRW f . fromAction
    where f s0 = (s0{localBlockApply=reason}, \s -> s{localBlockApply=localBlockApply s0})

unsafeAllowApply :: Action a -> Action a
unsafeAllowApply  = applyBlockedBy Nothing

blockApply :: String -> Action a -> Action a
blockApply = applyBlockedBy . Just

-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
--   You cannot depend on a rule (e.g. 'need') while a resource is held.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = do
    Global{..} <- Action getRO
    liftIO $ globalDiagnostic $ show r ++ " waiting to acquire " ++ show i
    offset <- liftIO offsetTime
    Action $ captureRAW $ \continue -> acquireResource r globalPool i $ continue $ Right ()
    res <- Action $ tryRAW $ fromAction $ blockApply ("Within withResource using " ++ show r) $ do
        offset <- liftIO offset
        liftIO $ globalDiagnostic $ show r ++ " acquired " ++ show i ++ " in " ++ showDuration offset
        Action $ modifyRW $ \s -> s{localDiscount = localDiscount s + offset}
        act
    liftIO $ releaseResource r globalPool i
    liftIO $ globalDiagnostic $ show r ++ " released " ++ show i
    Action $ either throwRAW return res

-- | A version of 'newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'newCache' instead.
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO act = do
    var {- :: Var (Map k (Fence (Either SomeException ([Depends],v)))) -} <- newVar Map.empty
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
                            addPool pool $ do offset <- liftIO offset; k $ Right (v,offset)
                case res of
                    Left err -> Action $ throwRAW err
                    Right (deps,v) -> do
                        Action $ modifyRW $ \s -> s{localDepends = deps <> localDepends s, localDiscount = localDiscount s + offset}
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
                            let deps = subtractDepends pre post
                            liftIO $ signalFence bar $ Right (deps, v)
                            return v


-- | Run an action but do not depend on anything the action uses.
--   A more general version of 'orderOnly'.
orderOnlyAction :: Action a -> Action a
orderOnlyAction act = Action $ do
    pre <- getsRW localDepends
    res <- fromAction act
    modifyRW $ \s -> s{localDepends=pre}
    return res
