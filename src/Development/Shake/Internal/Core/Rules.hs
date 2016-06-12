{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds #-}

module Development.Shake.Internal.Core.Rules(
    Rule(..), Rules, runRules,
    runStored, runExecute, runEqual,
    rule, action, withoutActions, alternatives, priority
    ) where

import Control.Applicative
import Data.Tuple.Extra
import Control.Monad.Extra
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Typeable
import Data.Function
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import System.IO.Extra
import Data.Monoid
import System.IO.Unsafe

import Development.Shake.Classes
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Value
import Development.Shake.Internal.Types
import Development.Shake.Internal.Errors
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
-- newtype File = File FilePath deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- newtype Modtime = Modtime Double deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- getFileModtime file = ...
--
-- instance Rule File Modtime where
--     storedValue _ (File x) = do
--         exists <- System.Directory.doesFileExist x
--         if exists then Just \<$\> getFileModtime x else return Nothing
--     equalValue _ _ t1 t2 =
--         if t1 == t2 then EqualCheap else NotEqual
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
class (ShakeValue key, ShakeValue value) => Rule key value where

    -- | /[Required]/ Retrieve the @value@ associated with a @key@, if available.
    --
    --   As an example for filenames/timestamps, if the file exists you should return 'Just'
    --   the timestamp, but otherwise return 'Nothing'.
    storedValue :: ShakeOptions -> key -> IO (Maybe value)

    -- | /[Optional]/ Equality check, with a notion of how expensive the check was.
    equalValue :: ShakeOptions -> key -> value -> value -> EqualCost
    equalValue _ _ v1 v2 = if v1 == v2 then EqualCheap else NotEqual


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Action value))

ruleKey :: (key -> Maybe (Action value)) -> key
ruleKey = err "ruleKey"

ruleValue :: (key -> Maybe (Action value)) -> value
ruleValue = err "ruleValue"


-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'.
--   Rules are combined with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
--   To define your own custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT SRules IO a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newRules :: SRules -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules -> SRules) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

runRules :: ShakeOptions -> Rules () -> IO ([Action ()], Map.HashMap TypeRep RuleInfo)
runRules opts (Rules r) = do
    srules <- execWriterT r
    registerWitnesses srules
    return (actions srules, createRuleinfo opts srules)


data SRules = SRules
    {actions :: [Action ()]
    ,rules :: Map.HashMap TypeRep{-k-} (TypeRep{-k-},TypeRep{-v-},[(Double,ARule)]) -- higher fst is higher priority
    }

instance Monoid SRules where
    mempty = SRules [] (Map.fromList [])
    mappend (SRules x1 x2) (SRules y1 y2) = SRules (x1++y1) (Map.unionWith f x2 y2)
        where f (k, v1, xs) (_, v2, ys)
                | v1 == v2 = (k, v1, xs ++ ys)
                | otherwise = unsafePerformIO $ errorIncompatibleRules k v1 v2

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend = liftA2 mappend


-- | Add a rule to build a key, returning an appropriate 'Action' if the @key@ matches,
--   or 'Nothing' otherwise.
--   All rules at a given priority must be disjoint on all used @key@ values, with at most one match.
--   Rules have priority 1 by default, which can be modified with 'priority'.
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
--
--   The 'priority' function obeys the invariants:
--
-- @
-- 'priority' p1 ('priority' p2 r1) === 'priority' p1 r1
-- 'priority' p1 (r1 >> r2) === 'priority' p1 r1 >> 'priority' p1 r2
-- @
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
--   Inside 'alternatives' the 'priority' of each rule is not used to determine which rule matches,
--   but the resulting match uses that priority compared to the rules outside the 'alternatives' block.
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


registerWitnesses :: SRules -> IO ()
registerWitnesses SRules{..} =
    forM_ (Map.elems rules) $ \(_, _, (_,ARule r):_) -> do
        registerWitness (ruleKey r) (ruleValue r)


createRuleinfo :: ShakeOptions -> SRules -> Map.HashMap TypeRep RuleInfo
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
            where rs2 = sets [(i, \k -> fmap newValue <$> r (fromKey k)) | (i,ARule r) <- rs]

        sets :: Ord a => [(a, b)] -> [[b]] -- highest to lowest
        sets = map snd . reverse . groupSort

runStored :: Map.HashMap TypeRep RuleInfo-> Key -> IO (Maybe Value)
runStored mp k = case Map.lookup (typeKey k) mp of
    Nothing -> return Nothing
    Just RuleInfo{..} -> stored k

runEqual :: Map.HashMap TypeRep RuleInfo -> Key -> Value -> Value -> EqualCost
runEqual mp k v1 v2 = case Map.lookup (typeKey k) mp of
    Nothing -> NotEqual
    Just RuleInfo{..} -> equal k v1 v2

runExecute :: Map.HashMap TypeRep RuleInfo -> Key -> Action Value
runExecute mp k = let tk = typeKey k in case Map.lookup tk mp of
    Nothing -> liftIO $ errorNoRuleToBuildType tk (Just $ show k) Nothing
    Just RuleInfo{..} -> execute k
