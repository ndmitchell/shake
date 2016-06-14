{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds #-}

module Development.Shake.Internal.Core.Rules(
    Rules, runRules,
    BuiltinRule(..), addBuiltinRule, defaultEqualValue,
    addUserRule, alternatives, priority,
    action, withoutActions
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

import Development.Shake.Classes
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Value
import Development.Shake.Internal.Types
import Development.Shake.Internal.Errors
import Prelude


---------------------------------------------------------------------
-- RULES

--   Define a pair of types that can be used by Shake rules.
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
-- addBuiltinRule Builtininstance Rule File Modtime where
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

-- | TODO: Docs
data BuiltinRule key value = BuiltinRule
    {storedValue :: ShakeOptions -> key -> IO (Maybe value)
        -- ^ /[Required]/ Retrieve the @value@ associated with a @key@, if available.
        --
        --   As an example for filenames/timestamps, if the file exists you should return 'Just'
        --   the timestamp, but otherwise return 'Nothing'.
    ,equalValue :: ShakeOptions -> key -> value -> value -> EqualCost
        -- ^ /[Optional]/ Equality check, with a notion of how expensive the check was.
        --   Use 'defaultEqualValue' if you do not want a different equality.
    }

-- | Default 'equalValue' field.
defaultEqualValue :: Eq value => ShakeOptions -> key -> value -> value -> EqualCost
defaultEqualValue _ _ v1 v2 = if v1 == v2 then EqualCheap else NotEqual


data BuiltinRule_ = forall key value . (ShakeValue key, ShakeValue value) => BuiltinRule_ (BuiltinRule key value)

data UserRule_ = forall key value . (Typeable key, Typeable value) => UserRule_ (key -> Maybe (Action value))

builtinKey :: BuiltinRule key value -> key
builtinKey = err "builtinKey"

builtinValue :: BuiltinRule key value -> value
builtinValue = err "builtinValue"

ruleKey :: (key -> Maybe (Action value)) -> key
ruleKey = err "ruleKey"


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
    ,builtinRules :: Map.HashMap TypeRep{-k-} BuiltinRule_
    ,userRules :: Map.HashMap TypeRep{-k-} [(Double,UserRule_)] -- higher fst is higher priority
    }

instance Monoid SRules where
    mempty = SRules [] Map.empty Map.empty
    mappend (SRules x1 x2 x3) (SRules y1 y2 y3) = SRules (x1++y1) (Map.unionWithKey f x2 y2) (Map.unionWith (++) x3 y3)
        where
            f k _ _ = err "Cannot call addBuiltinRule twice on the same key" -- TODO, proper error message


instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend = liftA2 mappend


-- | Add a rule to build a key, returning an appropriate 'Action' if the @key@ matches,
--   or 'Nothing' otherwise.
--   All rules at a given priority must be disjoint on all used @key@ values, with at most one match.
--   Rules have priority 1 by default, which can be modified with 'priority'.
addUserRule :: (Typeable key, Typeable value) => (key -> Maybe (Action value)) -> Rules ()
addUserRule r = newRules mempty{userRules = Map.singleton k [(1,UserRule_ r)]}
    where k = typeOf $ ruleKey r


-- | Add a builtin rule type.
addBuiltinRule :: (ShakeValue key, ShakeValue value) => BuiltinRule key value -> Rules ()
addBuiltinRule b = newRules mempty{builtinRules = Map.singleton k $ BuiltinRule_ b}
    where k = typeOf $ builtinKey b


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
priority i = modifyRules $ \s -> s{userRules = Map.map (map $ first $ const i) $ userRules s}


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
alternatives = modifyRules $ \r -> r{userRules = Map.map f $ userRules r}
    where
        f [] = []
        f xs = let (is,rs) = unzip xs in [(maximum is, foldl1' g rs)]

        g (UserRule_ a) (UserRule_ b) = UserRule_ $ \x -> a x `mplus` b2 x
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
    forM_ (Map.elems builtinRules) $ \(BuiltinRule_ r) -> do
        registerWitness (builtinKey r) (builtinValue r)


createRuleinfo :: ShakeOptions -> SRules -> Map.HashMap TypeRep RuleInfo
createRuleinfo opt SRules{..}
    | bad@(_:_) <- Map.keys $ userRules `Map.difference` builtinRules = err "user rules with no builtin rules" -- TODO
    | otherwise = flip Map.map builtinRules $ \(BuiltinRule_ (b :: BuiltinRule k v)) -> case Map.lookup (typeOf (undefined :: k)) userRules of
        user -> RuleInfo (stored b) (equal b) (execute b $ fromMaybe [] user) (typeOf (undefined :: v))
    where
        stored BuiltinRule{..} = fmap (fmap newValue) . storedValue opt . fromKey

        equal BuiltinRule{..} = \k v1 v2 -> equalValue opt (fromKey k) (fromValue v1) (fromValue v2)

        execute (BuiltinRule{} :: BuiltinRule key value) rs = \k -> case filter (not . null) $ map (mapMaybe ($ k)) rs2 of
               [r]:_ -> r
               rs -> liftIO $ errorMultipleRulesMatch (typeKey k) (show k) (length rs)
            where rs2 = sets [ (i, \k -> fmap newValue <$> r' (fromKey k))
                             | (i,UserRule_ r) <- rs
                             , let Just (r' :: key -> Maybe (Action value)) = cast r ]

        sets :: Ord a => [(a, b)] -> [[b]] -- highest to lowest
        sets = map snd . reverse . groupSort
