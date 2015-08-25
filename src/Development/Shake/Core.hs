{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Development.Shake.Core(
    run,
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue,
#endif
    Rule(..), Rules, rule, action, withoutActions, alternatives, priority,
    Action, actionOnException, actionFinally, apply, apply1, traced, getShakeOptions,
    trackUse, trackChange, trackAllow,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    Resource, newResource, newResourceIO, withResource, withResources, newThrottle, newThrottleIO,
    newCache, newCacheIO,
    unsafeExtraThread,
    -- Internal stuff
    rulesIO, runAfter, unsafeIgnoreDependencies,
    ) where

import Control.Exception.Extra
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Typeable
import Data.Function
import Data.Either.Extra
import Numeric.Extra
import Data.List
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
import General.Extra
import General.Concurrent
import General.Cleanup
import General.String
import Prelude


---------------------------------------------------------------------
-- RULES

#if __GLASGOW_HASKELL__ >= 704
-- | Define an alias for the six type classes required for things involved in Shake 'Development.Shake.Rule's.
--   This alias is only available in GHC 7.4 and above, and requires the @ConstraintKinds@ extension.
--
--   To define your own values meeting the necessary constraints it is convenient to use the extensions
--   @GeneralizedNewtypeDeriving@ and @DeriveDataTypeable@ to write:
--
-- > newtype MyType = MyType (String, Bool) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)
#endif


-- | Define a pair of types that can be used by Shake rules.
--   To import all the type classes required see "Development.Shake.Classes".
--
--   A 'Rule' instance for a class of artifacts (e.g. /files/) provides:
--
-- * How to identify individual artifacts, given by the @key@ type, e.g. with file names.
--
-- * How to describe the state of an artifact, given by the @value@ type, e.g. the file modification time.
--
-- * A way to compare two states of the same individual artifact, with 'equalValue' returning either
--   'EqualCheap' or 'NotEqual'.
--
-- * A way to query the current state of an artifact, with 'storedValue' returning the current state,
--   or 'Nothing' if there is no current state (e.g. the file does not exist).
--
--   Checking if an artifact needs to be built consists of comparing two @value@s
--   of the same @key@ with 'equalValue'. The first value is obtained by applying
--   'storedValue' to the @key@ and the second is the value stored in the build
--   database after the last successful build.
--
--   As an example, below is a simplified rule for building files, where files are identified
--   by a 'FilePath' and their state is identified by a hash of their contents
--   (the builtin functions 'Development.Shake.need' and 'Development.Shake.%>'
--   provide a similar rule).
--
-- @
--newtype File = File FilePath deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
--newtype Modtime = Modtime Double deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
--getFileModtime file = ...
--
--instance Rule File Modtime where
--    storedValue _ (File x) = do
--        exists <- System.Directory.doesFileExist x
--        if exists then Just \<$\> getFileModtime x else return Nothing
--    equalValue _ _ t1 t2 =
--        if t1 == t2 then EqualCheap else NotEqual
-- @
--
--   This example instance means:
--
-- * A value of type @File@ uniquely identifies a generated file.
--
-- * A value of type @Modtime@ will be used to check if a file is up-to-date.
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
--         compile :: File -> 'Action' Modtime
--         compile (File outputFile) = do
--             -- figure out the name of the input file
--             let inputFile = outputFile '<.>' \"foo\"
--             'unit' $ 'Development.Shake.cmd' \"fooCC\" inputFile outputFile
--             -- return the (new) file modtime of the output file:
--             getFileModtime outputFile
-- @
--
--   /Note:/ In this example, the timestamps of the input files are never
--   used, let alone compared to the timestamps of the ouput files.
--   Dependencies between output and input files are /not/ expressed by
--   'Rule' instances. Dependencies are created automatically by 'apply'.
--
--   For rules whose values are not stored externally,
--   'storedValue' should return 'Just' with a sentinel value
--   and 'equalValue' should always return 'EqualCheap' for that sentinel.
class (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key, ShakeValue value
#else
    Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key,
    Show value, Typeable value, Eq value, Hashable value, Binary value, NFData value
#endif
    ) => Rule key value where

    -- | /[Required]/ Retrieve the @value@ associated with a @key@, if available.
    --
    --   As an example for filenames/timestamps, if the file exists you should return 'Just'
    --   the timestamp, but otherwise return 'Nothing'.
    storedValue :: ShakeOptions -> key -> IO (Maybe value)

    -- | /[Optional]/ Equality check, with a notion of how expensive the check was.
    equalValue :: ShakeOptions -> key -> value -> value -> EqualCost
    equalValue _ _ v1 v2 = if v1 == v2 then EqualCheap else NotEqual


data ARule m = forall key value . Rule key value => ARule (key -> Maybe (m value))

ruleKey :: (key -> Maybe (m value)) -> key
ruleKey = err "ruleKey"

ruleValue :: (key -> Maybe (m value)) -> value
ruleValue = err "ruleValue"


-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'. Rules are combined
--   with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation. To define your own
--   custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT (SRules Action) IO a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Monad, Functor, Applicative, MonadFix)

rulesIO :: IO a -> Rules a
rulesIO = Rules . liftIO

newRules :: SRules Action -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules Action -> SRules Action) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

getRules :: Rules () -> IO (SRules Action)
getRules (Rules r) = execWriterT r


data SRules m = SRules
    {actions :: [m ()]
    ,rules :: Map.HashMap TypeRep{-k-} (TypeRep{-k-},TypeRep{-v-},[(Double,ARule m)]) -- higher fst is higher priority
    }

instance Monoid (SRules m) where
    mempty = SRules [] (Map.fromList [])
    mappend (SRules x1 x2) (SRules y1 y2) = SRules (x1++y1) (Map.unionWith f x2 y2)
        where f (k, v1, xs) (_, v2, ys)
                | v1 == v2 = (k, v1, xs ++ ys)
                | otherwise = unsafePerformIO $ errorIncompatibleRules k v1 v2

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend = liftA2 mappend


-- | Add a rule to build a key, returning an appropriate 'Action'. All rules at a given priority
--   must be disjoint. Rules have priority 1 by default, but can be modified with 'priority'.
rule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
rule r = newRules mempty{rules = Map.singleton k (k, v, [(1,ARule r)])}
    where k = typeOf $ ruleKey r; v = typeOf $ ruleValue r


-- | Change the priority of a given set of rules, where higher priorities take precedence.
--   All matching rules at a given priority must be disjoint, or an error is raised.
--   All builtin Shake rules have priority between 0 and 1.
--   Excessive use of 'priority' is discouraged. As an example:
--
-- @
-- 'priority' 4 $ \"hello.*\" %> \\out -> 'writeFile'' out \"hello.*\"
-- 'priority' 8 $ \"*.txt\" %> \\out -> 'writeFile'' out \"*.txt\"
-- @
--
--   In this example @hello.txt@ will match the second rule, instead of raising an error about ambiguity.
priority :: Double -> Rules () -> Rules ()
priority i = modifyRules $ \s -> s{rules = Map.map (\(a,b,cs) -> (a,b,map (first $ const i) cs)) $ rules s}


-- | Change the matching behaviour of rules so rules do not have to be disjoint, but are instead matched
--   in order. Only recommended for small blocks containing a handful of rules.
--
-- @
-- 'alternatives' $ do
--     \"hello.*\" %> \\out -> 'writeFile'' out \"hello.*\"
--     \"*.txt\" %> \\out -> 'writeFile'' out \"*.txt\"
-- @
--
--   In this example @hello.txt@ will match the first rule, instead of raising an error about ambiguity.
alternatives :: Rules () -> Rules ()
alternatives = modifyRules $ \r -> r{rules = Map.map f $ rules r}
    where
        f (k, v, []) = (k, v, [])
        f (k, v, xs) = let (is,rs) = unzip xs in (k, v, [(maximum is, foldl1' g rs)])

        g (ARule a) (ARule b) = ARule $ \x -> a x `mplus` b2 x
            where b2 = fmap (fmap (fromJust . cast)) . b . fromJust . cast


-- | Run an action, usually used for specifying top-level requirements.
--
-- @
-- main = 'Development.Shake.shake' 'shakeOptions' $ do
--    'action' $ do
--        b <- 'Development.Shake.doesFileExist' \"file.src\"
--        when b $ 'Development.Shake.need' [\"file.out\"]
-- @
--
--   This 'action' builds @file.out@, but only if @file.src@ exists. The 'action'
--   will be run in every build execution (unless 'withoutActions' is used), so only cheap
--   operations should be performed. All arguments to 'action' may be run in parallel, in any order.
--
--   For the standard requirement of only 'Development.Shake.need'ing a fixed list of files in the 'action',
--   see 'Development.Shake.want'.
action :: Action a -> Rules ()
action a = newRules mempty{actions=[void a]}


-- | Remove all actions specified in a set of rules, usually used for implementing
--   command line specification of what to build.
withoutActions :: Rules () -> Rules ()
withoutActions = modifyRules $ \x -> x{actions=[]}


registerWitnesses :: SRules m -> IO ()
registerWitnesses SRules{..} =
    forM_ (Map.elems rules) $ \(_, _, (_,ARule r):_) -> do
        registerWitness $ ruleKey r
        registerWitness $ ruleValue r


data RuleInfo m = RuleInfo
    {stored :: Key -> IO (Maybe Value)
    ,equal :: Key -> Value -> Value -> EqualCost
    ,execute :: Key -> m Value
    ,resultType :: TypeRep
    }

createRuleinfo :: ShakeOptions -> SRules Action -> Map.HashMap TypeRep (RuleInfo Action)
createRuleinfo opt SRules{..} = flip Map.map rules $ \(_,tv,rs) -> RuleInfo (stored rs) (equal rs) (execute rs) tv
    where
        stored ((_,ARule r):_) = fmap (fmap newValue) . f r . fromKey
            where f :: Rule key value => (key -> Maybe (m value)) -> (key -> IO (Maybe value))
                  f _ = storedValue opt

        equal ((_,ARule r):_) = \k v1 v2 -> f r (fromKey k) (fromValue v1) (fromValue v2)
            where f :: Rule key value => (key -> Maybe (m value)) -> key -> value -> value -> EqualCost
                  f _ = equalValue opt

        execute rs = \k -> case filter (not . null) $ map (mapMaybe ($ k)) rs2 of
               [r]:_ -> r
               rs -> liftIO $ errorMultipleRulesMatch (typeKey k) (show k) (length rs)
            where rs2 = sets [(i, \k -> fmap (fmap newValue) $ r (fromKey k)) | (i,ARule r) <- rs]

        sets :: Ord a => [(a, b)] -> [[b]] -- highest to lowest
        sets = map (map snd) . reverse . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

runStored :: Map.HashMap TypeRep (RuleInfo m) -> Key -> IO (Maybe Value)
runStored mp k = case Map.lookup (typeKey k) mp of
    Nothing -> return Nothing
    Just RuleInfo{..} -> stored k

runEqual :: Map.HashMap TypeRep (RuleInfo m) -> Key -> Value -> Value -> EqualCost
runEqual mp k v1 v2 = case Map.lookup (typeKey k) mp of
    Nothing -> NotEqual
    Just RuleInfo{..} -> equal k v1 v2

runExecute :: MonadIO m => Map.HashMap TypeRep (RuleInfo m) -> Key -> m Value
runExecute mp k = let tk = typeKey k in case Map.lookup tk mp of
    Nothing -> liftIO $ errorNoRuleToBuildType tk (Just $ show k) Nothing
    Just RuleInfo{..} -> execute k


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
    }


-- local variables of Action
data Local = Local
    -- constants
    {localStack :: Stack
    -- stack scoped local variables
    ,localVerbosity :: Verbosity
    ,localBlockApply ::  Maybe String -- reason to block apply, or Nothing to allow
    -- mutable local variables
    ,localDepends :: [Depends] -- built up in reverse
    ,localDiscount :: !Seconds
    ,localTraces :: [Trace] -- in reverse
    ,localTrackAllows :: [Key -> Bool]
    ,localTrackUsed :: [Key]
    }

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'rule' and 'action'. The 'Action' monad tracks the dependencies of a 'Rule'.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO)


actionBoom :: Bool -> Action a -> IO b -> Action a
actionBoom runOnSuccess act clean = do
    cleanup <- Action $ getsRO globalCleanup
    clean <- liftIO $ addCleanup cleanup $ void clean
    res <- Action $ catchRAW (fromAction act) $ \e -> liftIO (clean True) >> throwRAW e
    liftIO $ clean runOnSuccess
    return res

-- | If an exception is raised by the 'Action', perform some 'IO'.
actionOnException :: Action a -> IO b -> Action a
actionOnException = actionBoom False

-- | After an 'Action', perform some 'IO', even if there is an exception.
actionFinally :: Action a -> IO b -> Action a
actionFinally = actionBoom True


-- | Internal main function (not exported publicly)
run :: ShakeOptions -> Rules () -> IO ()
run opts@ShakeOptions{..} rs = (if shakeLineBuffering then lineBuffering else id) $ do
    opts@ShakeOptions{..} <- if shakeThreads /= 0 then return opts else do p <- getProcessorCount; return opts{shakeThreads=p}

    start <- offsetTime
    rs <- getRules rs
    registerWitnesses rs

    outputLocked <- do
        lock <- newLock
        return $ \v msg -> withLock lock $ shakeOutput v msg

    let diagnostic = if shakeVerbosity >= Diagnostic then outputLocked Diagnostic . ("% "++) else const $ return ()
    let output v = outputLocked v . abbreviate shakeAbbreviations

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
            now <- getCurrentDirectory
            when (dir /= now) $ errorStructured
                "Lint checking error - current directory has changed"
                [("When", Just msg)
                ,("Wanted",Just dir)
                ,("Got",Just now)]
                ""

    after <- newIORef []
    absent <- newIORef []
    withCleanup $ \cleanup -> do
        _ <- addCleanup cleanup $ do
            when shakeTimings printTimings
            resetTimings -- so we don't leak memory
        withNumCapabilities shakeThreads $ do
            withDatabase opts diagnostic $ \database -> do
                wait <- newBarrier
                tid <- forkIO $ flip finally (signalBarrier wait ()) $
                    shakeProgress $ do
                        failure <- fmap (fmap fst) $ readIORef except
                        stats <- progress database
                        return stats{isFailure=failure}
                addCleanup cleanup $ do
                    killThread tid
                    void $ timeout 1000000 $ waitBarrier wait

                let ruleinfo = createRuleinfo opts rs
                addTiming "Running rules"
                runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                    let s0 = Global database pool cleanup start ruleinfo output opts diagnostic lint after absent
                    let s1 = Local emptyStack shakeVerbosity Nothing [] 0 [] [] []
                    forM_ (actions rs) $ \act -> do
                        addPool pool $ runAction s0 s1 act $ \x -> case x of
                            Left e -> raiseError =<< shakeException s0 (return ["Top-level action/want"]) e
                            Right x -> return x
                maybe (return ()) (throwIO . snd) =<< readIORef except

                when (null $ actions rs) $ do
                    when (shakeVerbosity >= Normal) $ output Normal "Warning: No want/action statements, nothing to do"

                when (isJust shakeLint) $ do
                    addTiming "Lint checking"
                    absent <- readIORef absent
                    checkValid database (runStored ruleinfo) (runEqual ruleinfo) absent
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


lineBuffering :: IO a -> IO a
lineBuffering = withBuffering stdout LineBuffering . withBuffering stderr LineBuffering


abbreviate :: [(String,String)] -> String -> String
abbreviate [] = id
abbreviate abbrev = f
    where
        -- order so longer appreviations are preferred
        ordAbbrev = sortBy (flip (compare `on` length . fst)) abbrev

        f [] = []
        f x | (to,rest):_ <- [(to,rest) | (from,to) <- ordAbbrev, Just rest <- [stripPrefix from x]] = to ++ f rest
        f (x:xs) = x : f xs


runAction :: Global -> Local -> Action a -> Capture (Either SomeException a)
runAction g l (Action x) k = runRAW g l x k


runAfter :: IO () -> Action ()
runAfter op = do
    Global{..} <- Action getRO
    liftIO $ atomicModifyIORef globalAfter $ \ops -> (op:ops, ())


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'rule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: Rule key value => [key] -> Action [value]
apply = f -- Don't short-circuit [] as we still want error messages
    where
        -- We don't want the forall in the Haddock docs
        f :: forall key value . Rule key value => [key] -> Action [value]
        f ks = do
            let tk = typeOf (err "apply key" :: key)
                tv = typeOf (err "apply type" :: value)
            Global{..} <- Action getRO
            block <- Action $ getsRW localBlockApply
            whenJust block $ liftIO . errorNoApply tk (fmap show $ listToMaybe ks)
            case Map.lookup tk globalRules of
                Nothing -> liftIO $ errorNoRuleToBuildType tk (fmap show $ listToMaybe ks) (Just tv)
                Just RuleInfo{resultType=tv2} | tv /= tv2 -> liftIO $ errorRuleTypeMismatch tk (fmap show $ listToMaybe ks) tv2 tv
                _ -> fmap (map fromValue) $ applyKeyValue $ map newKey ks


applyKeyValue :: [Key] -> Action [Value]
applyKeyValue [] = return []
applyKeyValue ks = do
    global@Global{..} <- Action getRO
    let exec stack k continue = do
            let s = Local {localVerbosity=shakeVerbosity globalOptions, localDepends=[], localStack=stack, localBlockApply=Nothing
                          ,localDiscount=0, localTraces=[], localTrackAllows=[], localTrackUsed=[]}
            let top = showTopStack stack
            time <- offsetTime
            runAction global s (do
                liftIO $ evaluate $ rnf k
                liftIO $ globalLint $ "before building " ++ top
                putWhen Chatty $ "# " ++ show k
                res <- runExecute globalRules k
                when (shakeLint globalOptions == Just LintTracker)
                    trackCheckUsed
                Action $ fmap ((,) res) getRW) $ \x -> case x of
                    Left e -> continue . Left . toException =<< shakeException global (showStack globalDatabase stack) e
                    Right (res, Local{..}) -> do
                        dur <- time
                        globalLint $ "after building " ++ top
                        let ans = (res, reverse localDepends, dur - localDiscount, reverse localTraces)
                        evaluate $ rnf ans
                        continue $ Right ans
    stack <- Action $ getsRW localStack
    (dur, dep, vs) <- Action $ captureRAW $ build globalPool globalDatabase (Ops (runStored globalRules) (runEqual globalRules) exec) stack ks
    Action $ modifyRW $ \s -> s{localDiscount=localDiscount s + dur, localDepends=dep : localDepends s}
    return vs


-- | Turn a normal exception into a ShakeException, giving it a stack and printing it out if in staunch mode.
--   If the exception is already a ShakeException (e.g. it's a child of ours who failed and we are rethrowing)
--   then do nothing with it.
shakeException :: Global -> IO [String] -> SomeException -> IO ShakeException
shakeException Global{globalOptions=ShakeOptions{..},..} stk e@(SomeException inner) = case cast inner of
    Just e@ShakeException{} -> return e
    Nothing -> do
        stk <- stk
        e <- return $ ShakeException (last $ "Unknown call stack" : stk) stk e
        when (shakeStaunch && shakeVerbosity >= Quiet) $
            globalOutput Quiet $ show e ++ "Continuing due to staunch mode"
        return e


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: Rule key value => key -> Action value
apply1 = fmap head . apply . return


-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ getsRO globalOptions


-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'.
--   The trace list is used for profile reports (see 'shakeReport').
traced :: String -> IO a -> Action a
traced msg act = do
    Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    start <- liftIO globalTimestamp
    putNormal $ "# " ++ msg ++ " (for " ++ showTopStack stack ++ ")"
    res <- liftIO act
    stop <- liftIO globalTimestamp
    Action $ modifyRW $ \s -> s{localTraces = Trace (pack msg) (doubleToFloat start) (doubleToFloat stop) : localTraces s}
    return res


putWhen :: Verbosity -> String -> Action ()
putWhen v msg = do
    Global{..} <- Action getRO
    verb <- getVerbosity
    when (verb >= v) $
        liftIO $ globalOutput v msg


-- | Write a message to the output when the verbosity ('shakeVerbosity') is appropriate.
--   The output will not be interleaved with any other Shake messages
--   (other than those generated by system commands).
putLoud, putNormal, putQuiet :: String -> Action ()
putLoud = putWhen Loud
putNormal = putWhen Normal
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
trackUse ::
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key
#else
    (Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key)
#endif
    => key -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) you have already been used by apply, and are on the dependency list
-- 3) someone explicitly gave you permission with trackAllow
-- 4) at the end of the rule, a) you are now on the dependency list, and b) this key itself has no dependencies (is source file)
trackUse key = do
    let k = newKey key
    Global{..} <- Action getRO
    l@Local{..} <- Action getRW
    deps <- liftIO $ concatMapM (listDepends globalDatabase) localDepends
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
        deps <- concatMapM (listDepends globalDatabase) localDepends

        -- check 3a
        bad <- return $ localTrackUsed \\ deps
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " used but not depended upon")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check 3b
        bad <- flip filterM localTrackUsed $ \k -> fmap (not . null) $ lookupDependencies globalDatabase k
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " depended upon after being used")
                [("Used", Just $ show x) | x <- bad]
                ""


-- | Track that a key has been changed by the action preceeding it.
trackChange ::
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key
#else
    (Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key)
#endif
    => key -> Action ()
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
trackAllow ::
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key
#else
    (Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key)
#endif
    => (key -> Bool) -> Action ()
trackAllow test = Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        -- We don't want the forall in the Haddock docs
        arrow1Type :: forall a b . Typeable a => (a -> b) -> TypeRep
        arrow1Type _ = typeOf (err "trackAllow" :: a)

        ty = arrow1Type test
        f k = typeKey k == ty && test (fromKey k)


---------------------------------------------------------------------
-- RESOURCES

-- | Create a finite resource, given a name (for error messages) and a quantity of the resource that exists.
--   Shake will ensure that actions using the same finite resource do not execute in parallel.
--   As an example, only one set of calls to the Excel API can occur at one time, therefore
--   Excel is a finite resource of quantity 1. You can write:
--
-- @
-- 'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=2} $ do
--    'Development.Shake.want' [\"a.xls\",\"b.xls\"]
--    excel <- 'Development.Shake.newResource' \"Excel\" 1
--    \"*.xls\" 'Development.Shake.%>' \\out ->
--        'Development.Shake.withResource' excel 1 $
--            'Development.Shake.cmd' \"excel\" out ...
-- @
--
--   Now the two calls to @excel@ will not happen in parallel.
--
--   As another example, calls to compilers are usually CPU bound but calls to linkers are usually
--   disk bound. Running 8 linkers will often cause an 8 CPU system to grid to a halt. We can limit
--   ourselves to 4 linkers with:
--
-- @
-- disk <- 'Development.Shake.newResource' \"Disk\" 4
-- 'Development.Shake.want' [show i 'Development.Shake.FilePath.<.>' \"exe\" | i <- [1..100]]
-- \"*.exe\" 'Development.Shake.%>' \\out ->
--     'Development.Shake.withResource' disk 1 $
--         'Development.Shake.cmd' \"ld -o\" [out] ...
-- \"*.o\" 'Development.Shake.%>' \\out ->
--     'Development.Shake.cmd' \"cl -o\" [out] ...
-- @
newResource :: String -> Int -> Rules Resource
newResource name mx = rulesIO $ newResourceIO name mx


-- | Create a throttled resource, given a name (for error messages) and a number of resources (the 'Int') that can be
--   used per time period (the 'Double' in seconds). Shake will ensure that actions using the same throttled resource
--   do not exceed the limits. As an example, let us assume that making more than 1 request every 5 seconds to
--   Google results in our client being blacklisted, we can write:
--
-- @
-- google <- 'Development.Shake.newThrottle' \"Google\" 1 5
-- \"*.url\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.withResource' google 1 $
--         'Development.Shake.cmd' \"wget\" [\"http:\/\/google.com?q=\" ++ 'Development.Shake.FilePath.takeBaseName' out] \"-O\" [out]
-- @
--
--   Now we will wait at least 5 seconds after querying Google before performing another query. If Google change the rules to
--   allow 12 requests per minute we can instead use @'Development.Shake.newThrottle' \"Google\" 12 60@, which would allow
--   greater parallelisation, and avoid throttling entirely if only a small number of requests are necessary.
--
--   In the original example we never make a fresh request until 5 seconds after the previous request has /completed/. If we instead
--   want to throttle requests since the previous request /started/ we can write:
--
-- @
-- google <- 'Development.Shake.newThrottle' \"Google\" 1 5
-- \"*.url\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.withResource' google 1 $ return ()
--     'Development.Shake.cmd' \"wget\" [\"http:\/\/google.com?q=\" ++ 'Development.Shake.FilePath.takeBaseName' out] \"-O\" [out]
-- @
--
--   However, the rule may not continue running immediately after 'Development.Shake.withResource' completes, so while
--   we will never exceed an average of 1 request every 5 seconds, we may end up running an unbounded number of
--   requests simultaneously. If this limitation causes a problem in practice it can be fixed.
newThrottle :: String -> Int -> Double -> Rules Resource
newThrottle name count period = rulesIO $ newThrottleIO name count period


blockApply :: String -> Action a -> Action a
blockApply msg = Action . unmodifyRW f . fromAction
    where f s0 = (s0{localBlockApply=Just msg}, \s -> s{localBlockApply=localBlockApply s0})


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


-- | Run an action which uses part of several finite resources. Acquires the resources in a stable
--   order, to prevent deadlock. If all rules requiring more than one resource acquire those
--   resources with a single call to 'withResources', resources will not deadlock.
withResources :: [(Resource, Int)] -> Action a -> Action a
withResources res act
    | (r,i):_ <- filter ((< 0) . snd) res = error $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show i
    | otherwise = f $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) res
    where
        f [] = act
        f (r:rs) = withResource (fst $ head r) (sum $ map snd r) $ f rs


-- | A version of 'newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'newCache' instead.
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO act = do
    var {- :: Var (Map k (Fence (Either SomeException ([Depends],v)))) -} <- newVar Map.empty
    return $ \key -> do
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

-- | Given an action on a key, produce a cached version that will execute the action at most once per key.
--   Using the cached result will still result include any dependencies that the action requires.
--   Each call to 'newCache' creates a separate cache that is independent of all other calls to 'newCache'.
--
--   This function is useful when creating files that store intermediate values,
--   to avoid the overhead of repeatedly reading from disk, particularly if the file requires expensive parsing.
--   As an example:
--
-- @
-- digits \<- 'newCache' $ \\file -> do
--     src \<- readFile\' file
--     return $ length $ filter isDigit src
-- \"*.digits\" 'Development.Shake.%>' \\x -> do
--     v1 \<- digits ('dropExtension' x)
--     v2 \<- digits ('dropExtension' x)
--     'Development.Shake.writeFile'' x $ show (v1,v2)
-- @
--
--   To create the result @MyFile.txt.digits@ the file @MyFile.txt@ will be read and counted, but only at most
--   once per execution.
newCache :: (Eq k, Hashable k) => (k -> Action v) -> Rules (k -> Action v)
newCache = rulesIO . newCacheIO


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources. Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot depend on a rule (e.g. 'need') while the extra thread is executing.
--   If the rule blocks (e.g. calls 'withResource') then the extra thread may be used by some other action.
--   Only really suitable for calling 'cmd'/'command'.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = Action $ do
    global@Global{..} <- getRO
    stop <- liftIO $ increasePool globalPool
    res <- tryRAW $ fromAction $ blockApply "Within unsafeExtraThread" act
    liftIO stop
    captureRAW $ \continue -> (if isLeft res then addPoolPriority else addPool) globalPool $ continue res


-- | Ignore any dependencies added by an action.
unsafeIgnoreDependencies :: Action a -> Action a
unsafeIgnoreDependencies act = Action $ do
    pre <- getsRW localDepends
    res <- fromAction act
    modifyRW $ \s -> s{localDepends=pre}
    return res
