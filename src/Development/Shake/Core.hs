{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds, DeriveFunctor #-}

module Development.Shake.Core(
    run,
    ShakeValue,
    Rules, action, withoutActions, alternatives, priority,
    BuiltinRule(..), BuiltinResult(..), addBuiltinRule,
    addUserRule, getUserRules, userRule, simpleCheck,
    Action, actionOnException, actionFinally, apply, apply1, traced, getShakeOptions, getProgress,
    trackUse, trackChange, trackAllow,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    Resource, newResource, newResourceIO, withResource, withResources, newThrottle, newThrottleIO,
    newCache, newCacheIO,
    unsafeExtraThread, unsafeAllowApply,
    parallel,
    orderOnlyAction,
    -- Internal stuff
    runAfter,
    ) where

import Control.Exception.Extra
import Control.Applicative
import Data.Tuple.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Dynamic
import General.Binary
import Data.Function
import Data.Either.Extra
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.IORef
import System.IO.Extra
import System.Time.Extra
import Data.Monoid

import Development.Shake.Classes
import Development.Shake.Core2
import Development.Shake.Errors
import Development.Shake.Pool
import Development.Shake.Monad
import Development.Shake.Resource
import Development.Shake.Value
import Development.Shake.Types
import General.Extra
import General.Cleanup
import Prelude

-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'.
--   Rules are combined with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
--   To define your own custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT (SRules Action) IO a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend = liftA2 mappend

newRules :: SRules Action -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules Action -> SRules Action) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

getRules :: Rules () -> IO (SRules Action)
getRules (Rules r) = execWriterT r

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
--  /[Required]/ Check if the @value@ associated with a @key@ is up-to-date.
--
--   As an example for filenames/timestamps, if the file exists with the given timestamp
--   you should return 'Continue', but otherwise return 'Rebuild'.
--   If the value has changed but you still do not want to rebuild, you should return
--   'Update' with the new value.
newBuiltinRule :: TypeRep -> BuiltinRule Action -> Rules ()
newBuiltinRule k r = newRules mempty{rules = Map.singleton k r}

-- | Less general version of newBuiltinRule
--   The passed-in function should return (new value, whether value changed, whether building was skipped)
addBuiltinRule :: (ShakeValue key, ShakeValue value) => (key -> Maybe value -> Bool -> Action (value, Bool, Bool)) -> Rules ()
addBuiltinRule = f
    where
    f :: forall key value. (ShakeValue key, ShakeValue value) => (key -> Maybe value -> Bool -> Action (value, Bool, Bool)) -> Rules ()
    f act = newBuiltinRule (typeOf (undefined :: key)) (BuiltinRule { execute = \k' vo' dep -> do
        let k = fromKeyDef k' (err "addBuiltinRule: incorrect type")
        let vo = fmap (decode . result) vo'
        (res, wasChanged, uptodate) <- act k vo dep
        return $ BuiltinResult
          { resultStoreB = encode res
          , resultValueB = toDyn res
          , dependsB = if uptodate then fmap depends vo' else Nothing
          , changedB = wasChanged
          }
    })


-- | A simplified built-in rule that runs on every Shake invocation, caches its value between runs, and uses Eq for equality.
simpleCheck :: (ShakeValue key, ShakeValue value) => (key -> Action value) -> Rules ()
simpleCheck = f
    where
    f :: forall key value. (ShakeValue key, ShakeValue value) => (key -> Action value) -> Rules ()
    f act = newBuiltinRule (typeOf (undefined :: key)) (BuiltinRule
        { execute = \k vo _ -> do
                --   | assume == AssumeDirty = return Rebuild
                --   | assume == AssumeSkip = return Continue
                --
            v <- act . fromKeyDef k $ err "simpleCheck key conversion failure"
            return $ BuiltinResult
              { resultStoreB = encode v
              , resultValueB = toDyn v
              , dependsB = Nothing
              , changedB = maybe False ((==) v . decode . result) vo
              }
        })

-- | Add a rule to build a key, returning an appropriate 'Action' if the @key@ matches,
--   or 'Nothing' otherwise. The 'Bool' is 'True' if the value changed.
--   All rules at a given priority must be disjoint on all used @key@ values, with at most one match.
--   Rules have priority 1 by default, which can be modified with 'priority'.
addUserRule :: (Typeable a) => a -> Rules ()
addUserRule r = newRules mempty{userrules = Map.singleton (typeOf r) (ARule (UserRule r))}

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
--
--   The 'priority' function obeys the invariants:
--
-- @
-- 'priority' p1 ('priority' p2 r1) === 'priority' p1 r1
-- 'priority' p1 (r1 >> r2) === 'priority' p1 r1 >> 'priority' p1 r2
-- @
priority :: Double -> Rules () -> Rules ()
priority d = modifyRules $ \s -> s{userrules = Map.map f $ userrules s}
  where f (ARule s) = ARule (Priority d s)

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
--   Inside 'alternatives' the 'priority' of each rule is not used to determine which rule matches,
--   but the resulting match uses that priority compared to the rules outside the 'alternatives' block.
alternatives :: Rules () -> Rules ()
alternatives = modifyRules $ \r -> r{userrules = Map.map f $ userrules r}
  where f (ARule s) = ARule (Alternative s)

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
    run' opts start rs

lineBuffering :: IO a -> IO a
lineBuffering = withBuffering stdout LineBuffering . withBuffering stderr LineBuffering

runAfter :: IO () -> Action ()
runAfter op = do
    Global{..} <- Action getRO
    liftIO $ atomicModifyIORef globalAfter $ \ops -> (op:ops, ())

-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: (ShakeValue key, ShakeValue value) => key -> Action value
apply1 = fmap head . apply . return

-- | Get the user rules for a given type
getUserRules :: (Typeable a) => Action (Maybe (UserRule a))
getUserRules = do
    umap <- Action $ getsRO globalUserRules
    let x = cast =<< Map.lookup (typeOf (fromJust x)) umap
    return x

-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ getsRO globalOptions

-- | Get the current 'Progress' structure, as would be returned by 'shakeProgress'.
getProgress :: Action Progress
getProgress = do
    res <- Action $ getsRO globalProgress
    liftIO res

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
newResource name mx = liftIO $ newResourceIO name mx


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
newThrottle name count period = liftIO $ newThrottleIO name count period

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
newCache = liftIO . newCacheIO


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources.
--   Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot depend on a rule (e.g. 'need') while the extra thread is executing.
--   If the rule blocks (e.g. calls 'withResource') then the extra thread may be used by some other action.
--   Only really suitable for calling 'cmd'/'command'.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = Action $ do
    Global{..} <- getRO
    stop <- liftIO $ increasePool globalPool
    res <- tryRAW $ fromAction $ blockApply "Within unsafeExtraThread" act
    liftIO stop
    captureRAW $ \continue -> (if isLeft res then addPoolPriority else addPool) globalPool $ continue res


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
    results :: [IORef (Maybe (Either SomeException a))] <- liftIO $ replicateM (length acts) $ newIORef Nothing

    captureRAW $ \continue -> do
        let resume = do
                res <- liftIO $ sequence . catMaybes <$> mapM readIORef results
                continue res

        liftIO $ forM_ (zip acts results) $ \(act, result) -> do
            let act2 = ifM (liftIO $ isJust <$> readVar todo) act (fail "")
            addPool globalPool $ runAction global local act2 $ \res -> do
                writeIORef result $ Just res
                modifyVar_ todo $ \v -> case v of
                    Nothing -> return Nothing
                    Just i | i == 1 || isLeft res -> do resume; return Nothing
                    Just i -> return $ Just $ i - 1
