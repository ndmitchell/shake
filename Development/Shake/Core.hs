{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Development.Shake.Core(
    run,
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue,
#endif
    Rule(..), Rules, defaultRule, rule, action, withoutActions,
    Action, actionOnException, actionFinally, apply, apply1, traced, getShakeOptions,
    trackUse, trackChange,
    getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    Resource, newResource, newResourceIO, withResource, withResources, newThrottle, newThrottleIO,
    unsafeExtraThread,
    -- Internal stuff
    rulesIO, runAfter
    ) where

import Control.Exception as E
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict as State
import Data.Typeable
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.IORef
import System.Directory
import System.IO

import Development.Shake.Classes
import Development.Shake.Pool
import Development.Shake.Database
import Development.Shake.Resource
import Development.Shake.Value
import Development.Shake.Report
import Development.Shake.Types
import Development.Shake.Errors
import General.Timing
import General.Base


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
class (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key, ShakeValue value
#else
    Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key,
    Show value, Typeable value, Eq value, Hashable value, Binary value, NFData value
#endif
    ) => Rule key value where

    -- | Retrieve the @value@ associated with a @key@, if available.
    --
    --   As an example for filenames/timestamps, if the file exists you should return 'Just'
    --   the timestamp, but otherwise return 'Nothing'. For rules whose values are not
    --   stored externally, 'storedValue' should return 'Nothing'.
    storedValue :: key -> IO (Maybe value)


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Action value))

ruleKey :: Rule key value => (key -> Maybe (Action value)) -> key
ruleKey = err "ruleKey"

ruleValue :: Rule key value => (key -> Maybe (Action value)) -> value
ruleValue = err "ruleValue"


-- | Define a set of rules. Rules can be created with calls to functions such as '*>' or 'action'. Rules are combined
--   with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation. To define your own
--   custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT SRules IO a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Monad, Functor, Applicative)

rulesIO :: IO a -> Rules a
rulesIO = Rules . liftIO

newRules :: SRules -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules -> SRules) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

getRules :: Rules () -> IO SRules
getRules (Rules r) = execWriterT r


data SRules = SRules
    {actions :: [Action ()]
    ,rules :: Map.HashMap TypeRep{-k-} (TypeRep{-k-},TypeRep{-v-},[(Int,ARule)]) -- higher fst is higher priority
    }

instance Monoid SRules where
    mempty = SRules [] (Map.fromList [])
    mappend (SRules x1 x2) (SRules y1 y2) = SRules (x1++y1) (Map.unionWith f x2 y2)
        where f (k, v1, xs) (_, v2, ys)
                | v1 == v2 = (k, v1, xs ++ ys)
                | otherwise = errorIncompatibleRules k v1 v2

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend a b = do a <- a; b <- b; return $ mappend a b


-- | Like 'rule', but lower priority, if no 'rule' exists then 'defaultRule' is checked.
--   All default rules must be disjoint.
defaultRule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
defaultRule = rulePriority 0


-- | Add a rule to build a key, returning an appropriate 'Action'. All rules must be disjoint.
--   To define lower priority rules use 'defaultRule'.
rule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
rule = rulePriority 1


-- | Add a rule at a given priority, higher numbers correspond to higher-priority rules.
--   The function 'defaultRule' is priority 0 and 'rule' is priority 1. All rules of the same
--   priority must be disjoint.
rulePriority :: Rule key value => Int -> (key -> Maybe (Action value)) -> Rules ()
rulePriority i r = newRules mempty{rules = Map.singleton k (k, v, [(i,ARule r)])}
    where k = typeOf $ ruleKey r; v = typeOf $ ruleValue r


-- | Track that a key has been used by the action preceeding it.
trackUse ::
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key
#else
    (Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key)
#endif
    => key -> Action ()
trackUse _ = return ()


-- | Track that a key has been changed by the action preceeding it.
trackChange ::
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue key
#else
    (Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key)
#endif
    => key -> Action ()
trackChange _ = return ()


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
action a = newRules mempty{actions=[a >> return ()]}


-- | Remove all actions specified in a set of rules, usually used for implementing
--   command line specification of what to build.
withoutActions :: Rules () -> Rules ()
withoutActions = modifyRules $ \x -> x{actions=[]}


---------------------------------------------------------------------
-- MAKE

data RuleInfo = RuleInfo
    {stored :: Key -> IO (Maybe Value)
    ,execute :: Key -> Action Value
    ,resultType :: TypeRep
    }

data SAction = SAction
    -- global constants
    {database :: Database
    ,pool :: Pool
    ,timestamp :: IO Time
    ,ruleinfo :: Map.HashMap TypeRep RuleInfo
    ,output :: Verbosity -> String -> IO ()
    ,opts :: ShakeOptions
    ,diagnostic :: String -> IO ()
    ,lint :: String -> IO ()
    ,after :: IORef [IO ()]
    -- stack variables
    ,stack :: Stack
    -- local variables
    ,verbosity :: Verbosity
    ,depends :: [Depends] -- built up in reverse
    ,discount :: !Duration
    ,traces :: [Trace] -- in reverse
    ,blockapply ::  Maybe String -- reason to block apply, or Nothing to allow
    }

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'rule' and 'action'. The 'Action' monad tracks the dependencies of a 'Rule'.
newtype Action a = Action (StateT SAction IO a)
    deriving (Monad, MonadIO, Functor, Applicative)


-- | If an exception is raised by the 'Action', perform some 'IO'.
actionOnException :: Action a -> IO b -> Action a
actionOnException act clean = do
    s <- Action State.get
    (res,s) <- liftIO $ onException (runAction s act) clean
    Action $ State.put s
    return res


-- | After an 'Action', perform some 'IO', even if there is an exception.
actionFinally :: Action a -> IO b -> Action a
actionFinally act clean = do
    res <- actionOnException act clean
    liftIO clean
    return res


-- | Internal main function (not exported publicly)
run :: ShakeOptions -> Rules () -> IO ()
run opts@ShakeOptions{..} rs = (if shakeLineBuffering then lineBuffering else id) $ do
    start <- offsetTime
    rs <- getRules rs
    registerWitnesses rs

    outputLocked <- do
        lock <- newLock
        return $ \v msg -> withLock lock $ shakeOutput v msg

    let diagnostic = if shakeVerbosity >= Diagnostic then outputLocked Diagnostic . ("% "++) else const $ return ()
    let output v = outputLocked v . abbreviate shakeAbbreviations

    except <- newIORef (Nothing :: Maybe (String, SomeException))
    let staunch act | not shakeStaunch = act >> return ()
                    | otherwise = do
            res <- try act
            case res of
                Left err -> do
                    let named = maybe "unknown rule" shakeExceptionTarget . cast
                    atomicModifyIORef except $ \v -> (Just $ fromMaybe (named err, err) v, ())
                    let msg = show err ++ "Continuing due to staunch mode, this error will be repeated later"
                    when (shakeVerbosity >= Quiet) $ output Quiet msg
                Right _ -> return ()

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

    progressThread <- newIORef Nothing
    after <- newIORef []
    let cleanup = do
            flip whenJust killThread =<< readIORef progressThread
            when shakeTimings printTimings
            resetTimings -- so we don't leak memory
    flip finally cleanup $
        withCapabilities shakeThreads $ do
            withDatabase opts diagnostic $ \database -> do
                tid <- forkIO $ shakeProgress $ do
                    failure <- fmap (fmap fst) $ readIORef except
                    stats <- progress database
                    return stats{isFailure=failure}
                writeIORef progressThread $ Just tid
                let ruleinfo = createRuleinfo rs
                addTiming "Running rules"
                runPool (shakeThreads == 1) shakeThreads $ \pool -> do
                    let s0 = SAction database pool start ruleinfo output opts diagnostic lint after emptyStack shakeVerbosity [] 0 [] Nothing
                    mapM_ (addPool pool . staunch . runAction s0) (actions rs)

                when (isJust shakeLint) $ do
                    addTiming "Lint checking"
                    checkValid database (runStored ruleinfo)
                    when (shakeVerbosity >= Loud) $ output Loud "Lint checking succeeded"
                when (isJust shakeReport) $ do
                    addTiming "Profile report"
                    let file = fromJust shakeReport
                    json <- showJSON database
                    when (shakeVerbosity >= Normal) $
                        output Normal $ "Writing HTML report to " ++ file
                    buildReport json file
            maybe (return ()) (throwIO . snd) =<< readIORef except
            sequence_ . reverse =<< readIORef after


withCapabilities :: Int -> IO a -> IO a
#if __GLASGOW_HASKELL__ >= 706
withCapabilities new act = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act
#else
withCapabilities new act = act
#endif

lineBuffering :: IO a -> IO a
lineBuffering = f stdout . f stderr
    where
        f h act = do
            bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
                hSetBuffering h LineBuffering
                act


abbreviate :: [(String,String)] -> String -> String
abbreviate [] = id
abbreviate abbrev = f
    where
        -- order so longer appreviations are preferred
        ordAbbrev = reverse $ sortBy (compare `on` length . fst) abbrev

        f [] = []
        f x | (to,rest):_ <- [(to,rest) | (from,to) <- ordAbbrev, Just rest <- [stripPrefix from x]] = to ++ f rest
        f (x:xs) = x : f xs


wrapStack :: IO [String] -> IO a -> IO a
wrapStack stk act = E.catch act $ \(SomeException e) -> case cast e of
    Just s@ShakeException{} -> throwIO s
    Nothing -> do
        stk <- stk
        if null stk then throwIO e
         else throwIO $ ShakeException (last stk) stk $ SomeException e


registerWitnesses :: SRules -> IO ()
registerWitnesses SRules{..} =
    forM_ (Map.elems rules) $ \(_, _, (_,ARule r):_) -> do
        registerWitness $ ruleKey r
        registerWitness $ ruleValue r


createRuleinfo :: SRules -> Map.HashMap TypeRep RuleInfo
createRuleinfo SRules{..} = flip Map.map rules $ \(_,tv,rs) -> RuleInfo (stored rs) (execute rs) tv
    where
        stored ((_,ARule r):_) = fmap (fmap newValue) . f r . fromKey
            where f :: Rule key value => (key -> Maybe (Action value)) -> (key -> IO (Maybe value))
                  f _ = storedValue

        execute rs = \k -> case filter (not . null) $ map (mapMaybe ($ k)) rs2 of
               [r]:_ -> r
               rs -> errorMultipleRulesMatch (typeKey k) (show k) (length rs)
            where rs2 = sets [(i, \k -> fmap (fmap newValue) $ r (fromKey k)) | (i,ARule r) <- rs] 

        sets :: Ord a => [(a, b)] -> [[b]] -- highest to lowest
        sets = map (map snd) . reverse . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

runStored :: Map.HashMap TypeRep RuleInfo -> Key -> IO (Maybe Value)
runStored mp k = case Map.lookup (typeKey k) mp of
    Nothing -> return Nothing
    Just RuleInfo{..} -> stored k

runExecute :: Map.HashMap TypeRep RuleInfo -> Key -> Action Value
runExecute mp k = let tk = typeKey k in case Map.lookup tk mp of
    Nothing -> errorNoRuleToBuildType tk (Just $ show k) Nothing -- Not sure if this is even possible, but best be safe
    Just RuleInfo{..} -> execute k


runAction :: SAction -> Action a -> IO (a, SAction)
runAction s (Action x) = runStateT x s


runAfter :: IO () -> Action ()
runAfter op = do
    s <- Action State.get
    liftIO $ atomicModifyIORef (after s) $ \ops -> (op:ops, ())


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'rule' or 'defaultRule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: Rule key value => [key] -> Action [value]
apply = f
    where
        -- We don't want the forall in the Haddock docs
        f :: forall key value . Rule key value => [key] -> Action [value]
        f ks = do
            let tk = typeOf (err "apply key" :: key)
                tv = typeOf (err "apply type" :: value)
            ruleinfo <- Action $ State.gets ruleinfo
            block <- Action $ State.gets blockapply
            whenJust block $ errorNoApply tk (fmap show $ listToMaybe ks)
            case Map.lookup tk ruleinfo of
                Nothing -> errorNoRuleToBuildType tk (fmap show $ listToMaybe ks) (Just tv)
                Just RuleInfo{resultType=tv2} | tv /= tv2 -> errorRuleTypeMismatch tk (fmap show $ listToMaybe ks) tv2 tv
                _ -> fmap (map fromValue) $ applyKeyValue $ map newKey ks


applyKeyValue :: [Key] -> Action [Value]
applyKeyValue ks = do
    s <- Action State.get
    let exec stack k = try $ wrapStack (showStack (database s) stack) $ do
            evaluate $ rnf k
            let s2 = s{verbosity=shakeVerbosity $ opts s, depends=[], stack=stack, discount=0, traces=[]}
            let top = topStack stack
            lint s $ "before building " ++ top
            (dur,(res,s2)) <- duration $ runAction s2 $ do
                putWhen Chatty $ "# " ++ show k
                runExecute (ruleinfo s) k
            lint s $ "after building " ++ top
            let ans = (res, reverse $ depends s2, dur - discount s2, reverse $ traces s2)
            evaluate $ rnf ans
            return ans
    res <- liftIO $ build (pool s) (database s) (Ops (runStored (ruleinfo s)) exec) (stack s) ks
    case res of
        Left err -> throw err
        Right (dur, dep, vs) -> do
            Action $ State.modify $ \s -> s{discount=discount s + dur, depends=dep : depends s}
            return vs


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: Rule key value => key -> Action value
apply1 = fmap head . apply . return


-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ gets opts


-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'.
--   The trace list is used for profile reports (see 'shakeReport').
traced :: String -> IO a -> Action a
traced msg act = do
    s <- Action State.get
    start <- liftIO $ timestamp s
    putNormal $ "# " ++ msg ++ " " ++ topStack (stack s)
    res <- liftIO act
    stop <- liftIO $ timestamp s
    Action $ State.modify $ \s -> s{traces = (pack msg,start,stop):traces s}
    return res


putWhen :: Verbosity -> String -> Action ()
putWhen v msg = do
    s <- Action State.get
    when (verbosity s >= v) $
        liftIO $ output s v msg


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
getVerbosity = Action $ gets verbosity


-- | Run an action with a particular verbosity level.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not have any impact on 'Diagnostic' tracing.
withVerbosity :: Verbosity -> Action a -> Action a
withVerbosity new act = do
    old <- Action $ State.gets verbosity
    Action $ State.modify $ \s -> s{verbosity=new}
    res <- act
    Action $ State.modify $ \s -> s{verbosity=old}
    return res


-- | Run an action with 'Quiet' verbosity, in particular messages produced by 'traced'
--   (including from 'Development.Shake.cmd' or 'Development.Shake.command') will not be printed to the screen.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not turn off any 'Diagnostic' tracing.
quietly :: Action a -> Action a
quietly = withVerbosity Quiet


-- | Create a finite resource, given a name (for error messages) and a quantity of the resource that exists.
--   Shake will ensure that actions using the same finite resource do not execute in parallel.
--   As an example, only one set of calls to the Excel API can occur at one time, therefore
--   Excel is a finite resource of quantity 1. You can write:
--
-- @
-- 'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=2} $ do
--    'Development.Shake.want' [\"a.xls\",\"b.xls\"]
--    excel <- 'Development.Shake.newResource' \"Excel\" 1
--    \"*.xls\" 'Development.Shake.*>' \\out ->
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
-- \"*.exe\" 'Development.Shake.*>' \\out ->
--     'Development.Shake.withResource' disk 1 $
--         'Development.Shake.cmd' \"ld -o\" [out] ...
-- \"*.o\" 'Development.Shake.*>' \\out ->
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
-- \"*.url\" 'Development.Shake.*>' \\out -> do
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
-- \"*.url\" 'Development.Shake.*>' \\out -> do
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
blockApply msg act = do
    s0 <- Action State.get
    Action $ State.put s0{blockapply=Just msg}
    res <- act
    Action $ State.modify $ \s -> s{blockapply=blockapply s0}
    return res


-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
--   You cannot call 'apply' / 'need' while the resource is acquired.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = do
    s <- Action State.get
    (res,s) <- liftIO $ bracket_
        (do res <- acquireResource r i
            case res of
                Nothing -> diagnostic s $ show r ++ " acquired " ++ show i ++ " with no wait"
                Just wait -> do
                    diagnostic s $ show r ++ " waiting to acquire " ++ show i
                    blockPool (pool s) $ fmap ((,) False) wait
                    diagnostic s $ show r ++ " acquired " ++ show i ++ " after waiting")
        (do releaseResource r i
            diagnostic s $ show r ++ " released " ++ show i)
        (runAction s $ blockApply ("Within withResource using " ++ show r) act)
    Action $ State.put s
    return res


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


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources. Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot call 'apply' / 'Development.Shake.need' while the extra thread is executing.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = do
    s <- Action State.get
    (res,s) <- liftIO $ blockPool (pool s) $ fmap ((,) False) $ runAction s act
    Action $ State.put s
    return res
