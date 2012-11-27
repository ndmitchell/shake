{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}

module Development.Shake.Core(
    ShakeOptions(..), shakeOptions, run,
    Rule(..), Rules, defaultRule, rule, action, withoutActions,
    Action, apply, apply1, traced,
    Verbosity(..), getVerbosity, putLoud, putNormal, putQuiet,
    Resource, newResource, withResource
    ) where

import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Binary(Binary)
import Data.Data
import Data.Hashable
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.Monoid

import Development.Shake.Pool
import Development.Shake.Database
import Development.Shake.Locks
import Development.Shake.Value
import Development.Shake.Report

---------------------------------------------------------------------
-- OPTIONS

-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=Just \"report.html\"} @
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath -- ^ Where shall I store the database and journal files (defaults to @.shake@).
    ,shakeThreads :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@).
                         --   To enable parallelism you may need to compile with @-threaded@.
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force a complete rebuild (defaults to @1@).
    ,shakeVerbosity :: Verbosity -- ^ What messages to print out (defaults to 'Normal').
    ,shakeStaunch :: Bool -- ^ Operate in staunch mode, where building continues even after errors (defaults to 'False').
    ,shakeReport :: Maybe FilePath -- ^ Produce an HTML profiling report (defaults to 'Nothing').
    ,shakeLint :: Bool -- ^ Perform basic sanity checks after building (defaults to 'False').
    ,shakeDeterministic :: Bool -- ^ Build files in a deterministic order, as far as possible
    }
    deriving (Show,Eq,Ord,Typeable,Data)

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions ".shake" 1 1 Normal False Nothing False False


-- | All foreseen exception conditions thrown by Shake, such problems with the rules or errors when executing
--   rules, will be raised using this exception type.
data ShakeException = ShakeException
        [String] -- Entries on the stack, starting at the top of the stack.
        SomeException -- Inner exception that was raised.
        -- If I make these Haddock comments, then Haddock dies
    deriving Typeable

instance Exception ShakeException

instance Show ShakeException where
    show (ShakeException stack inner) = unlines $
        "Error when running Shake build system:" :
        map ("* " ++) stack ++
        [show inner]


-- | The verbosity data type, used by 'shakeVerbosity'.
data Verbosity
    = Silent -- ^ Don't print any messages.
    | Quiet  -- ^ Only print essential messages (typically errors).
    | Normal -- ^ Print normal messages (typically errors and warnings).
    | Loud   -- ^ Print lots of messages (typically errors, warnings and status updates).
    | Diagnostic -- ^ Print messages for virtually everything (for debugging a build system).
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Typeable,Data)


---------------------------------------------------------------------
-- RULES

-- | Define a pair of types that can be used by Shake rules.
class (
    Show key, Typeable key, Eq key, Hashable key, Binary key, NFData key,
    Show value, Typeable value, Eq value, Hashable value, Binary value, NFData value
    ) => Rule key value | key -> value where

    -- | Given that the database contains @key@/@value@, does that still match the on-disk contents?
    --
    --   As an example for filenames/timestamps, if the file exists and had the same timestamp, you
    --   would return 'True', but otherwise return 'False'. For rule values which are not also stored
    --   on disk, 'validStored' should always return 'True'.
    validStored :: key -> value -> IO Bool
    validStored _ _ = return True

{-
    -- | Return 'True' if the value should not be changed by the build system. Defaults to returning
    --   'False'. Only used when running with 'shakeLint'.
    invariant :: key -> Bool
    invariant _ = False

    -- | Given an action, return what has changed, along with what you think should
    --   have stayed the same. Only used when running with 'shakeLint'.
    observed :: IO a -> IO (Observed key, a)
    observed = fmap ((,) mempty)


-- | Determine what was observed to change. For each field @Nothing@ means you don't know anything, while
--   @Just []@ means you know that nothing was changed/used.
data Observed a = Observed
    {changed :: Maybe [a] -- ^ A list of keys which had their value altered.
    ,used :: Maybe [a] -- ^ A list of keys whose value was used.
    }
    deriving (Show,Eq,Ord)

instance Functor Observed where
    fmap f (Observed a b) = Observed (g a) (g b)
        where g = fmap (map f)

instance Monoid (Observed a) where
    mempty = Observed Nothing Nothing
    mappend (Observed x1 y1) (Observed x2 y2) = Observed (f x1 x2) (f y1 y2)
        where
            f Nothing Nothing = Nothing
            f a b = Just $ fromMaybe [] a ++ fromMaybe [] b
-}


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Action value))

ruleKey :: Rule key value => (key -> Maybe (Action value)) -> key
ruleKey = undefined

ruleValue :: Rule key value => (key -> Maybe (Action value)) -> value
ruleValue = undefined

ruleStored :: Rule key value => (key -> Maybe (Action value)) -> (key -> value -> IO Bool)
ruleStored _ = validStored


-- | Define a set of rules. Rules can be created with calls to 'rule', 'defaultRule' or 'action'. Rules are combined
--   with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
data Rules a = Rules
    {value :: a -- not really used, other than for the Monad instance
    ,actions :: [Action ()]
    ,rules :: Map.HashMap TypeRep{-k-} (TypeRep{-k-},TypeRep{-v-},[(Int,ARule)]) -- higher fst is higher priority
    }

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend a b = (a >> b){value = value a `mappend` value b}

instance Monad Rules where
    return x = Rules x [] (Map.fromList [])
    Rules v1 x1 x2 >>= f = case f v1 of
        Rules v2 y1 y2 -> Rules v2 (x1++y1) (Map.unionWith g x2 y2)
        where g (k, v1, xs) (_, v2, ys)
                | v1 == v2 = (k, v1, xs ++ ys)
                | otherwise = error $ "There are two incompatible rules for " ++ show k ++ ", producing " ++ show v1 ++ " and " ++ show v2

instance Functor Rules where
    fmap f x = return . f =<< x


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
rulePriority i r = mempty{rules = Map.singleton k (k, v, [(i,ARule r)])}
    where k = typeOf $ ruleKey r; v = typeOf $ ruleValue r


-- | Run an action, usually used for specifying top-level requirements.
action :: Action a -> Rules ()
action a = mempty{actions=[a >> return ()]}


-- | Remove all actions specified in a set of rules, usually used for implementing
--   command line specification of what to build.
withoutActions :: Rules () -> Rules ()
withoutActions x = x{actions=[]}


---------------------------------------------------------------------
-- MAKE

data S = S
    -- global constants
    {database :: Database
    ,pool :: Pool
    ,started :: IO Time
    ,stored :: Key -> Value -> IO Bool
    ,execute :: Key -> Action Value
    ,output :: String -> IO ()
    ,verbosity :: Verbosity
    ,logger :: String -> IO ()
    -- stack variables
    ,stack :: Stack
    -- local variables
    ,depends :: [Depends] -- built up in reverse
    ,discount :: Duration
    ,traces :: [(String, Time, Time)] -- in reverse
    }

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'need' to execute files.
--   Action values are used by 'rule' and 'action'.
newtype Action a = Action (StateT S IO a)
    deriving (Functor, Monad, MonadIO)


-- | Internal main function (not exported publicly)
run :: ShakeOptions -> Rules () -> IO ()
run opts@ShakeOptions{..} rs = do
    start <- startTime
    registerWitnesses rs

    output <- do
        lock <- newLock
        return $ withLock lock . putStrLn

    let logger = if shakeVerbosity >= Diagnostic then output . ("% "++) else const $ return ()

    except <- newVar (Nothing :: Maybe SomeException)
    let staunch act | not shakeStaunch = act >> return ()
                    | otherwise = do
            res <- try act
            case res of
                Left err -> do
                    modifyVar_ except $ \v -> return $ Just $ fromMaybe err v
                    let msg = show err ++ "Continuing due to staunch mode, this error will be repeated later"
                    when (shakeVerbosity >= Quiet) $ output msg
                Right _ -> return ()

    let stored = createStored rs
    let execute = createExecute rs
    withDatabase logger shakeFiles shakeVersion $ \database -> do
        runPool shakeDeterministic shakeThreads $ \pool -> do
            let s0 = S database pool start stored execute output shakeVerbosity logger emptyStack [] 0 []
            mapM_ (addPool pool . staunch . wrapStack (return []) . runAction s0) (actions rs)
        when shakeLint $ do
            checkValid database stored
            when (shakeVerbosity >= Loud) $ output "Lint checking succeeded"
        when (isJust shakeReport) $ do
            let file = fromJust shakeReport
            json <- showJSON database
            when (shakeVerbosity >= Normal) $
                putStrLn $ "Writing HTML report to " ++ file
            buildReport json file
    maybe (return ()) throwIO =<< readVar except


wrapStack :: IO [String] -> IO a -> IO a
wrapStack stk act = E.catch act $ \(SomeException e) -> case cast e of
    Just s@ShakeException{} -> throw s
    Nothing -> do
        stk <- stk
        throw $ ShakeException stk $ SomeException e


registerWitnesses :: Rules () -> IO ()
registerWitnesses Rules{..} =
    forM_ (Map.elems rules) $ \(_, _, (_,ARule r):_) -> do
        registerWitness $ ruleKey r
        registerWitness $ ruleValue r


createStored :: Rules () -> (Key -> Value -> IO Bool)
createStored Rules{..} = \k v ->
    let (tk,tv) = (typeKey k, typeValue v) in
    case Map.lookup tk mp of
        Nothing -> error $
            "Error: couldn't find instance Rule " ++ show tk ++ " " ++ show tv ++
            ", perhaps you are missing a call to defaultRule/rule?"
        Just (tv2,_) | tv2 /= tv -> error $
            "Error: couldn't find instance Rule " ++ show tk ++ " " ++ show tv ++
            ", but did find an instance Rule " ++ show tk ++ " " ++ show tv2 ++
            ", perhaps you have the types wrong in your call to apply?"
        Just (_, r) -> r k v
    where
        mp = flip Map.map rules $ \(k,v,(_,ARule r):_) -> (v, \kx vx -> ruleStored r (fromKey kx) (fromValue vx))


createExecute :: Rules () -> (Key -> Action Value)
createExecute Rules{..} = \k ->
    let tk = typeKey k in
    case Map.lookup tk mp of
        Nothing -> error $
            "Error: couldn't find any rules to build " ++ show k ++ " of type " ++ show tk ++
            ", perhaps you are missing a call to defaultRule/rule?"
        Just rs -> case filter (not . null) $ map (mapMaybe ($ k)) rs of
           [r]:_ -> r
           rs ->
              let s = if null rs then "no" else show (length $ head rs)
              in error $ "Error: " ++ s ++ " rules match for Rule " ++ show k ++ " of type " ++ show tk
    where
        mp = flip Map.map rules $ \(_,_,rs) -> sets [(i, \k -> fmap (fmap newValue) $ r (fromKey k)) | (i,ARule r) <- rs]

        sets :: Ord a => [(a, b)] -> [[b]]
        sets = map (map snd) . reverse . groupBy ((==) `on` fst) . sortBy (compare `on` fst)


runAction :: S -> Action a -> IO (a, S)
runAction s (Action x) = runStateT x s


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'rule' or 'defaultRule'.
apply :: Rule key value => [key] -> Action [value]
apply ks = fmap (map fromValue) $ applyKeyValue $ map newKey ks

applyKeyValue :: [Key] -> Action [Value]
applyKeyValue ks = Action $ do
    s <- get
    let exec stack k = try $ wrapStack (showStack (database s) stack) $ do
            evaluate $ rnf k
            let s2 = s{depends=[], stack=stack, discount=0, traces=[]}
            (dur,(res,s2)) <- duration $ runAction s2 $ do
                putNormal $ "# " ++ show k
                execute s k
            let ans = (res, reverse $ depends s2, dur - discount s2, reverse $ traces s2)
            evaluate $ rnf ans
            return ans
    res <- liftIO $ build (pool s) (database s) (Ops (stored s) exec) (stack s) ks
    case res of
        Left err -> throw err
        Right (dur, dep, vs) -> do
            modify $ \s -> s{discount=discount s + dur, depends=dep : depends s}
            return vs


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: Rule key value => key -> Action value
apply1 = fmap head . apply . return


-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Develoment.Shake.system'' command automatically calls 'traced'. The trace list is used for profile reports
--   (see 'shakeReport').
traced :: String -> IO a -> Action a
traced msg act = Action $ do
    s <- get
    start <- liftIO $ started s
    res <- liftIO act
    stop <- liftIO $ started s
    modify $ \s -> s{traces = (msg,start,stop):traces s}
    return res


putWhen :: (Verbosity -> Bool) -> String -> Action ()
putWhen f msg = Action $ do
    s <- get
    when (f $ verbosity s) $
        liftIO $ output s msg


-- | Write a message to the output when the verbosity ('shakeVerbosity') is appropriate.
--   The output will not be interleaved with any other Shake messages
--   (other than those generated by system commands).
putLoud, putNormal, putQuiet :: String -> Action ()
putLoud = putWhen (>= Loud)
putNormal = putWhen (>= Normal)
putQuiet = putWhen (>= Quiet)


-- | Get the current verbosity level, as set by 'shakeVerbosity'. If you
--   want to output information to the console, you are recommended to use
--   'putLoud' \/ 'putNormal' \/ 'putQuiet', which ensures multiple messages are
--   not interleaved.
getVerbosity :: Action Verbosity
getVerbosity = Action $ gets verbosity


-- | Run an action which uses part of a finite resource. For an example see 'Resource'.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = Action $ do
    s <- get
    (res,s) <- liftIO $ bracket_
        (do res <- acquireResource r i
            case res of
                Nothing -> logger s $ show r ++ " acquired " ++ show i ++ " with no wait"
                Just wait -> do
                    logger s $ show r ++ " waiting to acquire " ++ show i
                    blockPool (pool s) $ fmap ((,) False) wait
                    logger s $ show r ++ " acquired " ++ show i ++ " after waiting")
        (do releaseResource r i
            logger s $ show r ++ " released " ++ show i)
        (runAction s act)
    put s
    return res
