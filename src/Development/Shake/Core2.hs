{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, DeriveFunctor, ConstraintKinds, Rank2Types #-}

module Development.Shake.Core2(
    Action(..), runAction, Global(..), Local,
    Rule(..), ARule(..), AnalysisResult(..),
    SRules(..), RuleType(..), ruleKey, ruleValue,
    run', apply, applied, blockApply, unsafeAllowApply, withResource, newCacheIO,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    traced, trackUse, trackChange, trackAllow, orderOnlyAction,
    ) where

import Control.Exception.Extra
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Typeable
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
import Development.Shake.Special
import General.Timing
import General.Concurrent
import General.Cleanup
import General.String
import Prelude


---------------------------------------------------------------------
-- RULES

-- | Define a pair of types that can be used by Shake rules.
--   To import all the type classes required see "Development.Shake.Classes".
--
--   A 'Rule' instance for a class of artifacts (e.g. /files/) provides:
--
-- * How to identify individual artifacts, given by the @key@ type, e.g. with file names.
--
-- * How to describe the state of an artifact, given by the @value@ type, e.g. the file modification time.
--
-- * A way to compare an old state of the artifact with the current state of the artifact, 'analyseR'.
--
--   As an example, below is a simplified rule for building files, where files are identified
--   by a 'FilePath' and their state is identified by a hash of their contents
--   (the builtin functions 'Development.Shake.need' and 'Development.Shake.%>'
--   provide a similar rule).
--
-- @
-- newtype File = File FilePath deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- newtype Modtime = Modtime Double deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- getFileModtime file = ...
--
-- instance Rule File Modtime where
--     analyseR _ (File x) (Modtime d) = do
--         exists <- System.Directory.doesFileExist x
--         if exists then do
--             d2 <- getFileModtime x
--             return $ if d == d2 then Rebuild else Continue
--         else return Rebuild
-- @
--
--   This example instance means:
--
-- * A value of type @File@ uniquely identifies a generated file.
--
-- * A value of type @Modtime@ will be used to check if a file is up-to-date.
--
-- * A missing file will always rebuild, as in Make.
--
--   It is important to distinguish 'Rule' instances from actual /rules/. 'Rule'
--   instances are one component required for the creation of rules.
--   Actual /rules/ are functions from a @key@ to an 'Action'; they are
--   added to 'Rules' using the 'rule' function.
--
--   A rule can be created for the instance above with:
--
-- @
-- -- Compile foo files; for every foo output file there must be a
-- -- single input file named \"filename.foo\".
-- compileFoo :: 'Rules' ()
-- compileFoo = 'rule' (Just . compile)
--     where
--         compile :: File -> Maybe Modtime -> 'Action' (Modtime, Bool)
--         compile (File outputFile) oldd = do
--             -- figure out the name of the input file
--             let inputFile = outputFile '<.>' \"foo\"
--             'unit' $ 'Development.Shake.cmd' \"fooCC\" inputFile outputFile
--             -- return the (new) file modtime of the output file:
--             d <- getFileModtime outputFile
--             return (d,Just d == oldd)
-- @
--
--   /Note:/ In this example, the timestamps of the input files are never
--   used, let alone compared to the timestamps of the output files.
--   Dependencies between output and input files are /not/ expressed by
--   'Rule' instances. Dependencies are created automatically by 'apply'.
--
--   For rules whose values are not stored externally,
--   'analyseR' should always return 'Continue'.
class (ShakeValue key, ShakeValue value) => Rule key value where

    -- | /[Required]/ Check if the @value@ associated with a @key@ is up-to-date.
    --
    --   As an example for filenames/timestamps, if the file exists with the given timestamp
    --   you should return 'Continue', but otherwise return 'Rebuild'.
    --   If the value has changed but you still do not want to rebuild, you should return
    --   'Update' with the new value.
    analyseR :: ShakeOptions -> key -> value -> IO (AnalysisResult value)

data ARule k v m = Rule k v => Priority [(Double,k -> Maybe (Maybe v -> m (v, Bool)))] -- higher fst is higher priority
                 | Custom (k -> v -> IO (AnalysisResult v)) (k -> Maybe v -> m (v, Bool))

combineRules :: ARule k v m -> ARule k v m -> Maybe (ARule k v m)
combineRules (Priority xs) (Priority ys) = Just $ Priority (xs ++ ys)
combineRules _ _ = Nothing

ruleKey :: ARule k v m -> k
ruleKey _ = err "ruleKey"

ruleValue :: ARule k v m -> v
ruleValue _ = err "ruleValue"

data RuleType m = forall k v. (ShakeValue k, ShakeValue v) => ARule (ARule k v m)

data SRules m = SRules
    {actions :: [m ()]
    ,rules :: Map.HashMap TypeRep{-k-} (TypeRep{-k-},RuleType m)
    }

instance Typeable m => Monoid (SRules m) where
    mempty = SRules [] (Map.fromList [])
    mappend (SRules x1 x2) (SRules y1 y2) = SRules (x1++y1) (Map.unionWith f x2 y2)
        where f (k, ARule x) (_, ARule y)
                  | Just y' <- cast y = case combineRules x y' of
                       Just c -> (k, ARule c)
                       Nothing -> unsafePerformIO $ errorMultipleRulesMatch k Nothing 2
                  | otherwise = unsafePerformIO $ errorIncompatibleRules k (typeOf $ ruleValue x) (typeOf $ ruleValue y)

registerWitnesses :: SRules m -> IO ()
registerWitnesses SRules{..} =
    forM_ (Map.elems rules) $ \(_, ARule r) -> do
        registerWitness $ ruleKey r
        registerWitness $ ruleValue r

data RuleInfo m = RuleInfo
    {analyse :: ShakeOptions -> Key -> Value -> IO (AnalysisResult Value)
    ,execute :: Key -> Maybe Value -> m (Value, Bool)
    ,resultType :: TypeRep
    }

analyseI :: (ShakeValue key, ShakeValue value) => ARule key value m -> ShakeOptions -> Key -> Value -> IO (AnalysisResult Value)
analyseI rule opt@(shakeAssume -> assume) (fromKey -> k :: key) (fromValue -> r :: value)
  | assume == AssumeDirty = return Rebuild
  | assume == AssumeSkip = return Continue
  | otherwise = fmap newValue <$> case rule of
        Priority _ -> analyseR opt k r
        Custom a _ -> a k r

executeI :: (ShakeValue key, ShakeValue value) => ARule key value Action -> Key -> Maybe Value -> Action (Value, Bool)
executeI rule (fromKey -> k :: key) (fmap fromValue -> vold :: Maybe value) = do
    first newValue <$> case rule of
        Priority rs -> case filter (not . null) $ map (mapMaybe ($ k)) $ sets rs of
            [r]:_ -> r vold
            rs -> liftIO $ errorMultipleRulesMatch (typeOf k) (Just $ show k) (length rs)
        Custom _ e -> e k vold
  where
    sets :: Ord a => [(a, b)] -> [[b]] -- highest to lowest
    sets = map snd . reverse . groupSort

createRuleinfo :: ShakeOptions -> SRules Action -> Map.HashMap TypeRep (RuleInfo Action)
createRuleinfo opt SRules{..} = flip Map.map rules $ \(_,ARule c) -> RuleInfo
    { analyse = analyseI c
    , execute = executeI c
    , resultType = case c of
          Priority _ -> typeOf (ruleValue c)
          Custom _ _ -> typeOf (ruleValue c)
    }

---------------------------------------------------------------------
-- MAKE

-- global constants of Action
data Global = Global
    {globalDatabase :: Database
    ,globalPool :: Pool
    ,globalCleanup :: Cleanup
    ,globalTimestamp :: IO Seconds
    ,globalRules :: Map.HashMap TypeRep (RuleInfo Action)
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

-- | Internal main function (not exported publicly)
run' :: ShakeOptions -> IO Seconds -> SRules Action -> IO ()
run' opts@ShakeOptions{..} start rs = do
    registerWitnesses rs

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

    lint <- if isNothing shakeLint then return $ const $ return () else do
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

                let ruleinfo = createRuleinfo opts rs
                addTiming "Running rules"
                runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                    let s0 = Global database pool cleanup start ruleinfo output opts diagnostic lint after absent getProgress forwards
                    let s1 = Local emptyStack shakeVerbosity Nothing mempty 0 [] [] []
                    forM_ (actions rs) $ \act ->
                        addPool pool $ runAction s0 s1 act $ \x -> case x of
                            Left e -> raiseError =<< shakeException s0 (return ["Top-level action/want"]) e
                            Right x -> return x
                maybe (return ()) (throwIO . snd) =<< readIORef except
                assertFinishedDatabase database

                when (null $ actions rs) $
                    when (shakeVerbosity >= Normal) $ output Normal "Warning: No want/action statements, nothing to do"

                when (isJust shakeLint) $ do
                    addTiming "Lint checking"
                    absent' <- readIORef absent
                    checkValid database absent' $ \ks -> do
                        bad <- newIORef []
                        runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                            let opts2 = opts{shakeAssume=case shakeAssume of AssumeSkip -> AssumeSkip; _ -> AssumeNothing}
                            let s0 = Global database pool cleanup start ruleinfo output opts2 diagnostic lint after absent getProgress forwards
                            forM_ ks $ \(key,v) -> case v of
                                Ready ro -> do
                                    stat <- analyseResult_ s0 key (result ro)
                                    let clean = case stat of { Continue -> True; _ -> False }
                                        reply now = do
                                            diagnostic $ "Checking if " ++ show key ++ " is " ++ show (result ro) ++ ", " ++ if now == Nothing then "passed" else "FAILED"
                                            whenJust now $ \now -> modifyIORef' bad ((key, result ro, now):)
                                    if specialAlwaysRebuilds (result ro) then
                                        -- skip phony rules
                                        if specialAlwaysChanges (result ro) then
                                            reply Nothing
                                         else
                                            runKey_ s0 key (Just ro) emptyStack (incStep $ built ro) $ \ans -> case ans of
                                                Error e -> raiseError =<< shakeException s0 (return ["Lint-checking"]) e
                                                Ready r | built r == changed r -> reply . Just . show $ result r
                                                _ -> reply Nothing
                                     else
                                        reply (if clean then Nothing else Just (show stat))
                                _ -> return ()
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
                    live <- listLive database
                    let liveFiles = [show k | k <- live, specialIsFileKey $ typeKey k]
                    forM_ shakeLiveFiles $ \file -> do
                        when (shakeVerbosity >= Normal) $
                            output Normal $ "Writing live list to " ++ file
                        (if file == "-" then putStr else writeFile file) $ unlines liveFiles
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
    case Map.lookup tk globalRules of
        Nothing -> liftIO $ errorNoRuleToBuildType tk (show <$> listToMaybe ks) (Just tv)
        Just RuleInfo{resultType=tv2} | tv /= tv2 -> liftIO $ errorRuleTypeMismatch tk (show <$> listToMaybe ks) tv2 tv
        _ -> fmap (map fromValue) $ applyKeyValue $ map newKey ks

applyKeyValue :: [Key] -> Action [Value]
applyKeyValue [] = return []
applyKeyValue ks = do
    global@Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    block <- Action $ getsRW localBlockApply
    (dur, dep, vs) <- Action $ captureRAW $ build globalPool globalDatabase (Ops (analyseResult_ global) (runKey_ global)) stack block ks
    Action $ modifyRW $ \s -> s{localDiscount=localDiscount s + dur, localDepends=dep <> localDepends s}
    return vs

analyseResult_ :: Global -> Key -> Value -> IO (AnalysisResult Value)
analyseResult_ global@Global{..} k r = do
    let tk = typeKey k
    case Map.lookup tk globalRules of
        Nothing -> liftIO $ errorNoRuleToBuildType tk (Just $ show k) Nothing
        Just RuleInfo{..} -> analyse globalOptions k r

runKey_ :: Global -> Key -> Maybe Result -> Stack -> Development.Shake.Database.Step -> Capture Status
runKey_ global@Global{..} k r stack step continue = do
    time <- offsetTime
    let s = Local stack (shakeVerbosity globalOptions) Nothing mempty 0 [] [] []
    let top = showTopStack stack
    runAction global s (do
        let tk = typeKey k
        case Map.lookup tk globalRules of
            Nothing -> liftIO $ errorNoRuleToBuildType tk (Just $ show k) Nothing
            Just RuleInfo{..} -> do
                liftIO $ evaluate $ rnf k
                liftIO $ globalLint $ "before building " ++ top
                putWhen Chatty $ "# " ++ show k
                res <- execute k (fmap result r)
                when (Just LintFSATrace == shakeLint globalOptions) trackCheckUsed
                Action $ fmap ((,) res) getRW) $ \x -> case x of
                    Left e -> continue . Error . toException =<< shakeException global (showStack globalDatabase stack) e
                    Right ((res,equal), Local{..}) -> do
                        dur <- time
                        globalLint $ "after building " ++ top
                        let ans = Result
                                    { result = res
                                    , depends = finalizeDepends localDepends
                                    , generatedBy = Nothing
                                    , changed = if equal then maybe step changed r else step
                                    , built = step
                                    , execution = doubleToFloat $ dur - localDiscount
                                    , traces = reverse localTraces
                                    }
                        evaluate $ rnf ans
                        continue $ Ready ans

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
    let k = newKey key
    Global{..} <- Action getRO
    l@Local{..} <- Action getRW
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
    liftIO $ do
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
    liftIO $ do
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
trackAllowForall test = Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        tk = typeOf (err "trackAllow key" :: key)
        f k = typeKey k == tk && test (fromKey k)

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
