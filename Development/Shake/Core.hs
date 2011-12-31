{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}

module Development.Shake.Core(
    ShakeOptions(..), shakeOptions, run,
    Rule(..), Rules, defaultRule, rule, action,
    Action, apply, apply1, traced, currentStack, currentRule,
    putLoud, putNormal, putQuiet,
    Observed(..)
    ) where

import Prelude hiding (catch)
import Control.Concurrent.ParallelIO.Local
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Binary(Binary)
import Data.Hashable
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Typeable
import System.IO.Unsafe

import Development.Shake.Database
import Development.Shake.Locks
import Development.Shake.Value


---------------------------------------------------------------------
-- OPTIONS

-- | Options to control 'shake'.
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath -- ^ Where shall I store the database and journal files (defaults to @.shake@).
    ,shakeParallel :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@).
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force a complete rebuild.
    ,shakeVerbosity :: Int -- ^ 1 = normal, 0 = quiet, 2 = loud.
    ,shakeLint :: Bool -- ^ Run under lint mode, when set implies 'shakeParallel' is @1@ (defaults to 'False').
    }
    deriving (Show, Eq, Ord, Read)

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions ".shake" 1 1 1 False


data ShakeException = ShakeException [Key] SomeException
     deriving Typeable

instance Exception ShakeException

instance Show ShakeException where
    show (ShakeException stack inner) = unlines $
        "Error when running Shake build system:" :
        map (("* " ++) . show) stack ++
        [show inner]


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

    -- | Return 'True' if the value should not be changed by the build system. Defaults to returning
    --   'False'.
    invariant :: key -> Bool
    invariant _ = False

    -- | Given an action, return what has changed, along with what you think should
    --   have stayed the same.
    observed :: IO () -> IO (Observed key)
    observed act = act >> return (Observed Nothing Nothing)


data Observed a = Observed
    {changed :: Maybe [a]
    ,used :: Maybe [a]
    }

instance Functor Observed where
    fmap f (Observed a b) = Observed (g a) (g b)
        where g = fmap (map f)

instance Monoid (Observed a) where
    mempty = Observed Nothing Nothing
    mappend (Observed x1 y1) (Observed x2 y2) = Observed (f x1 x2) (f y1 y2)
        where
            f Nothing Nothing = Nothing
            f a b = Just $ fromMaybe [] a ++ fromMaybe [] b


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Action value))

ruleKey :: Rule key value => (key -> Maybe (Action value)) -> key
ruleKey = undefined

ruleValue :: Rule key value => (key -> Maybe (Action value)) -> value
ruleValue = undefined

ruleStored :: Rule key value => (key -> Maybe (Action value)) -> (key -> value -> Bool)
ruleStored _ k v = unsafePerformIO $ validStored k v -- safe because of the invariants on validStored


-- | Define a set of rules. Rules can be created with calls to 'rule', 'defaultRule' or 'action'. Rules are combined
--   with either the 'Monoid' instance, or more commonly using the 'Monad' instance and @do@ notation.
data Rules a = Rules
    {value :: a -- not really used, other than for the Monad instance
    ,actions :: [Action ()]
    -- FIXME: Should be Map TypeRep{k} (TypeRep{v}, [(Int,ARule)])
    ,rules :: [(Int,ARule)] -- higher fst is higher priority
    }

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend a b = (a >> b){value = value a `mappend` value b}

instance Monad Rules where
    return x = Rules x [] []
    Rules v1 x1 x2 >>= f = Rules v2 (x1++y1) (x2++y2)
        where Rules v2 y1 y2 = f v1

instance Functor Rules where
    fmap f x = return . f =<< x


-- | Like 'rule', but lower priority, if no 'rule' exists then 'defaultRule' is checked.
--   All default rules must be disjoint.
defaultRule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
defaultRule r = mempty{rules=[(0,ARule r)]}


-- | Add a rule to build a key, returning an appropriate 'Action'. All rules must be disjoint.
--   To define lower priority rules use 'defaultRule'.
rule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
rule r = mempty{rules=[(1,ARule r)]}


-- | Run an action, usually used for specifying top-level requirements.
action :: Action a -> Rules ()
action a = mempty{actions=[a >> return ()]}


---------------------------------------------------------------------
-- MAKE

data S = S
    -- global constants
    {database :: Database
    ,pool :: Pool
    ,started :: UTCTime
    ,stored :: Key -> Value -> Bool
    ,execute :: Key -> Action Value
    ,outputLock :: Var ()
    ,verbosity :: Int
    ,observer :: Key -> IO () -> IO ()
    -- stack variables
    ,stack :: [Key] -- in reverse
    -- local variables
    ,depends :: [[Key]] -- built up in reverse
    ,discount :: Double
    ,traces :: [(String, Double, Double)] -- in reverse
    }

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'need' to execute files.
--   Action values are used by 'rule' and 'action'.
newtype Action a = Action (StateT S IO a)
    deriving (Functor, Monad, MonadIO)


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
run :: ShakeOptions -> Rules () -> IO ()
run opts@ShakeOptions{..} rules = do
    start <- getCurrentTime
    registerWitnesses rules
    outputLock <- newVar ()
    withDatabase shakeFiles shakeVersion $ \database -> do
        withObserver database $ \observer ->
            withPool (if shakeLint then 1 else shakeParallel) $ \pool -> do
                let s0 = S database pool start (createStored rules) (createExecute rules)
                           outputLock shakeVerbosity observer [] [] 0 []
                if shakeLint
                    then mapM_ (wrapStack [] . runAction s0 . applyKeyValue . return) =<< allKeys database
                    else parallel_ pool $ map (wrapStack [] . runAction s0) (actions rules)
    where
        withObserver database act
            | not shakeLint = act $ \key val -> val
            | otherwise = do
            keys <- allKeys database
            act $ \key val -> val
            {-
            get the observed ones and record them
            create an observer that looks at all the observation functions it can find
            run the action
            then figure out if the observations are consistent
            write any failures to a lint report
            -}


wrapStack :: [Key] -> IO a -> IO a
wrapStack stk act = catch act $ \(SomeException e) -> case cast e of
    Just s@ShakeException{} -> throw s
    Nothing -> throw $ ShakeException stk $ SomeException e


registerWitnesses :: Rules () -> IO ()
registerWitnesses Rules{..} =
    forM_ rules $ \(_, ARule r) -> do
        registerWitness $ ruleKey r
        registerWitness $ ruleValue r


createStored :: Rules () -> (Key -> Value -> Bool)
createStored Rules{..} = \k v ->
    let (tk,tv) = (typeKey k, typeValue v)
        msg = "Error: couldn't find instance Rule " ++ show tk ++ " " ++ show tv ++
              ", perhaps you are missing a call to defaultRule/rule?"
    in (fromMaybe (error msg) $ Map.lookup tk mp) k v
    where mp = Map.fromList
                   [ (typeOf $ ruleKey r, stored)
                   | (_,ARule r) <- rules
                   , let stored k v = ruleStored r (fromKey k) (fromValue v)]


createExecute :: Rules () -> (Key -> Action Value)
createExecute Rules{..} = \k ->
    let tk = typeKey k
        rs = fromMaybe [] $ Map.lookup tk mp
    in case filter (not . null) $ map (mapMaybe ($ k)) rs of
           [r]:_ -> r
           rs ->
              let s = if null rs then "no" else show (length $ head rs)
              in error $ "Error: " ++ s ++ " rules match for Rule " ++ show tk ++
                         ", with key " ++ show k
    where
        mp = Map.map (map (map snd) . reverse . groupBy ((==) `on` fst) . sortBy (compare `on` fst)) $ Map.fromListWith (++)
                 [ (typeOf $ ruleKey r, [(i,exec)])
                 | (i,ARule r) <- rules
                 , let exec k = fmap (fmap newValue) $ r (fromKey k)]


runAction :: S -> Action a -> IO (a, S)
runAction s (Action x) = runStateT x s


duration :: UTCTime -> UTCTime -> Double
duration start end = fromRational $ toRational $ end `diffUTCTime` start


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'rule' or 'defaultRule'.
apply :: Rule key value => [key] -> Action [value]
apply ks = fmap (map fromValue) $ applyKeyValue $ map newKey ks

applyKeyValue :: [Key] -> Action [Value]
applyKeyValue ks = Action $ do
    modify $ \s -> s{depends=ks:depends s}
    loop
    where
        loop = do
            s <- get
            res <- liftIO $ request (database s) (stored s) ks
            case res of
                Block (seen,act) -> do
                    let bad = intersect (stack s) seen
                    if not $ null bad
                        then error $ "Invalid rules, recursion detected when trying to build: " ++ show (head bad)
                        else discounted (liftIO $ extraWorkerWhileBlocked (pool s) act) >> loop
                Response vs -> return vs
                Execute todo -> do
                    discounted $ liftIO $ parallel_ (pool s) $ flip map todo $ \t ->
                        wrapStack (reverse $ t:stack s) $ observer s t $ do
                            start <- getCurrentTime
                            let s2 = s{depends=[], stack=t:stack s, discount=0, traces=[]}
                            (res,s2) <- runAction s2 $ do
                                putNormal $ "# " ++ show t
                                execute s t
                            evaluate $ rnf res
                            end <- getCurrentTime
                            let x = duration start end - discount s2
                            finished (database s) t res (reverse $ depends s2) x (reverse $ traces s2)
                    loop

        discounted x = do
            start <- liftIO getCurrentTime
            res <- x
            end <- liftIO getCurrentTime
            modify $ \s -> s{discount=discount s + duration start end}


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow the potential for parallelism.
apply1 :: Rule key value => key -> Action value
apply1 = fmap head . apply . return


-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'system'' command automatically calls 'traced'.
traced :: String -> IO a -> Action a
traced msg act = Action $ do
    start <- liftIO getCurrentTime
    res <- liftIO act
    stop <- liftIO getCurrentTime
    modify $ \s -> s{traces = (msg,duration (started s) start, duration (started s) stop):traces s}
    return res


-- | Get the stack of 'Key's for the current rule - usually used to improve error messages.
--   Returns '[]' if being run by 'action', otherwise returns the stack with the oldest rule
--   first.
currentStack :: Action [Key]
currentStack = Action $ fmap reverse $ gets stack


-- | Get the 'Key' for the currently executing rule - usally used to improve error messages.
--   Returns 'Nothing' if being run by 'action'.
currentRule = Action $ fmap listToMaybe $ gets stack


putWhen :: (Int -> Bool) -> String -> Action ()
putWhen f msg = Action $ do
    s <- get
    when (f $ verbosity s) $
        liftIO $ modifyVar_ (outputLock s) $ const $
            putStrLn msg


-- | Write a message to the output when the verbosity is appropriate.
--   The output will not be interleaved with any other Shake messages
--   (other than those generated by system commands).
putLoud, putNormal, putQuiet :: String -> Action ()
putLoud = putWhen (>= 2)
putNormal = putWhen (>= 1)
putQuiet = putWhen (>= 0)
