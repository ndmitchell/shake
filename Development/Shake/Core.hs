{-# LANGUAGE RecordWildCards, ExistentialQuantification, FunctionalDependencies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Development.Shake.Core(
    ShakeOptions(..), shakeOptions, runShake,
    Rule(..), Rules, defaultRule, rule, action,
    Action, apply, apply1, traced, currentRule,
    putLoud, putNormal, putQuiet
    ) where

import Control.Concurrent.ParallelIO.Local
import Control.Monad
import Control.Monad.State
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

data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath -- ^ Where shall I store the database and journal files (defaults to @.@)
    ,shakeParallelism :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@)
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force everyone to rebuild
    ,shakeVerbosity :: Int -- ^ 1 = normal, 0 = quiet, 2 = loud
    }

shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions "." 1 1 1


---------------------------------------------------------------------
-- RULES

class (
    Show key, Typeable key, Eq key, Hashable key, Binary key,
    Show value, Typeable value, Eq value, Hashable value, Binary value
    ) => Rule key value | key -> value where
    validStored :: key -> value -> IO Bool
    validStored _ _ = return True


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Action value))

ruleKey :: Rule key value => (key -> Maybe (Action value)) -> key
ruleKey = undefined

ruleValue :: Rule key value => (key -> Maybe (Action value)) -> value
ruleValue = undefined

ruleStored :: Rule key value => (key -> Maybe (Action value)) -> (key -> value -> Bool)
ruleStored _ k v = unsafePerformIO $ validStored k v -- safe because of the invariants on validStored


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


-- accumulate the Rule instances from defaultRule and rule, and put them in
-- if no rules to build something then it's cache instance is dodgy anyway
defaultRule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
defaultRule r = mempty{rules=[(0,ARule r)]}


rule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
rule r = mempty{rules=[(1,ARule r)]}


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
    -- stack variables
    ,stack :: [Key] -- in reverse
    -- local variables
    ,depends :: [[Key]] -- built up in reverse
    ,discount :: Double
    ,traces :: [(String, Double, Double)] -- in reverse
    }

newtype Action a = Action (StateT S IO a)
    deriving (Functor, Monad, MonadIO)


runShake :: ShakeOptions -> Rules () -> IO Double
runShake ShakeOptions{..} rules = do
    start <- getCurrentTime
    registerWitnesses rules
    database <- openDatabase shakeFiles shakeVersion
    outputLock <- newVar ()
    withPool shakeParallelism $ \pool -> do
        let state = S database pool start (createStored rules) (createExecute rules) outputLock shakeVerbosity [] [] 0 []
        parallel_ pool $ map (runAction state) (actions rules)
    closeDatabase database
    end <- getCurrentTime
    return $ duration start end


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


apply :: Rule key value => [key] -> Action [value]
apply ks = Action $ do
    modify $ \s -> s{depends=map newKey ks:depends s}
    loop
    where
        loop = do
            s <- get
            res <- liftIO $ request (database s) (stored s) $ map newKey ks
            case res of
                Block act -> discounted (liftIO act) >> loop
                Response vs -> return $ map fromValue vs
                Execute todo -> do
                    let bad = intersect (stack s) todo
                    if not $ null bad then
                        error $ unlines $ "Invalid rules, recursion detected:" :
                                          map (("  " ++) . show) (reverse (head bad:stack s))
                     else do
                        discounted $ liftIO $ parallel_ (pool s) $ flip map todo $ \t -> do
                            start <- getCurrentTime
                            let s2 = s{depends=[], stack=t:stack s, discount=0, traces=[]}
                            (res,s2) <- runAction s2 $ do
                                putNormal $ "# " ++ show t
                                execute s t
                            end <- getCurrentTime
                            let x = duration start end - discount s2
                            finished (database s) t res (reverse $ depends s2) x (reverse $ traces s2)
                        loop

        discounted x = do
            start <- liftIO getCurrentTime
            res <- x
            end <- liftIO getCurrentTime
            modify $ \s -> s{discount=discount s + duration start end}


apply1 :: Rule key value => key -> Action value
apply1 = fmap head . apply . return


traced :: String -> IO a -> Action a
traced msg act = Action $ do
    start <- liftIO getCurrentTime
    res <- liftIO act
    stop <- liftIO getCurrentTime
    modify $ \s -> s{traces = (msg,duration (started s) start, duration (started s) stop):traces s}
    return res


currentRule :: Action (Maybe Key)
currentRule = Action $ do
    s <- get
    return $ listToMaybe $ stack s


putWhen :: (Int -> Bool) -> String -> Action ()
putWhen f msg = Action $ do
    s <- get
    when (f $ verbosity s) $
        liftIO $ modifyVar_ (outputLock s) $ const $
            putStrLn msg


putLoud, putNormal, putQuiet :: String -> Action ()
putLoud = putWhen (>= 2)
putNormal = putWhen (>= 1)
putQuiet = putWhen (>= 0)
