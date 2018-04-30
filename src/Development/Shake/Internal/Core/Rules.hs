{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Core.Rules(
    Rules, runRules,
    RuleResult, addBuiltinRule, addBuiltinRuleEx,
    noLint, noIdentity,
    getShakeOptionsRules,
    getUserRulesVersioned, getUserRuleOne, getUserRuleList, getUserRuleMaybe,
    addUserRule, alternatives, priority, versioned,
    action, withoutActions
    ) where

import Control.Applicative
import Data.Tuple.Extra
import Control.Monad.Extra
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict
import Development.Shake.Classes
import General.Binary
import General.Extra
import Data.Typeable.Extra
import Data.Function
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import qualified General.TypeMap as TMap
import Data.Maybe
import System.IO.Extra
import Data.Semigroup (Semigroup (..))
import Data.Monoid hiding ((<>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary.Builder as Bin
import Data.Binary.Put
import Data.Binary.Get
import General.ListBuilder

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import Prelude


---------------------------------------------------------------------
-- RULES

-- | Are there any rules which use 'versioned'
getUserRulesVersioned :: Typeable a => proxy a -> Action Bool
getUserRulesVersioned (p :: proxy a) = do
    Global{..} <- Action getRO
    return $ maybe False userRuleVersioned (TMap.lookup globalUserRules :: Maybe (UserRuleVersioned a))


-- | Get the 'ShakeOptions' that were used.
getShakeOptionsRules :: Rules ShakeOptions
getShakeOptionsRules = Rules $ lift ask

-- | Get the user rules that were added at a particular type which return 'Just' on a given function.
--   Return all equally applicable rules, paired with the version of the rule
--   (set by 'versioned'). Where rules are specified with 'alternatives' or 'priority'
--   the less-applicable rules will not be returned.
--
--   If you can only deal with zero/one results, call 'getUserRuleMaybe' or 'getUserRuleOne',
--   which raise informative errors.
getUserRuleList :: Typeable a => (a -> Maybe b) -> Action [(Int, b)]
getUserRuleList test = do
    Global{..} <- Action getRO
    let rules = maybe mempty userRuleContents $ TMap.lookup globalUserRules
    return $ head $ (map snd $ reverse $ groupSort $ f (Version 0) Nothing $ fmap test rules) ++ [[]]
    where
        f :: Version -> Maybe Double -> UserRule (Maybe a) -> [(Double,(Int,a))]
        f (Version v) p (UserRule x) = maybe [] (\x -> [(fromMaybe 1 p,(v,x))]) x
        f v p (Unordered xs) = concatMap (f v p) xs
        f v p (Priority p2 x) = f v (Just $ fromMaybe p2 p) x
        f v p (Versioned v2 x) = f v2 p x
        f v p (Alternative x) = take 1 $ f v p x


-- | A version of 'getUserRuleList' that fails if there is more than one result
--   Requires a @key@ for better error messages.
getUserRuleMaybe :: (ShakeValue key, Typeable a) => key -> (a -> Maybe String) -> (a -> Maybe b) -> Action (Maybe (Int, b))
getUserRuleMaybe key disp test = do
    res <- getUserRuleList $ \x -> (,) x <$> test x
    case res of
        [] -> return Nothing
        [x] -> return $ Just $ second snd x
        xs -> throwM $ errorMultipleRulesMatch (typeOf key) (show key) (map (disp . fst . snd) xs)

-- | A version of 'getUserRuleList' that fails if there is not exactly one result
--   Requires a @key@ for better error messages.
getUserRuleOne :: (ShakeValue key, Typeable a) => key -> (a -> Maybe String) -> (a -> Maybe b) -> Action (Int, b)
getUserRuleOne key disp test = do
    res <- getUserRuleList $ \x -> (,) x <$> test x
    case res of
        [x] -> return $ second snd x
        xs -> throwM $ errorMultipleRulesMatch (typeOf key) (show key) (map (disp . fst . snd) xs)


-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'.
--   Rules are combined with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
--   To define your own custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT SRules (ReaderT ShakeOptions IO) a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newRules :: SRules -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules -> SRules) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

runRules :: ShakeOptions -> Rules () -> IO ([Action ()], Map.HashMap TypeRep BuiltinRule, TMap.Map UserRuleVersioned)
runRules opts (Rules r) = do
    SRules{..} <- runReaderT (execWriterT r) opts
    return (runListBuilder actions, builtinRules, userRules)

data SRules = SRules
    {actions :: !(ListBuilder (Action ()))
    ,builtinRules :: !(Map.HashMap TypeRep{-k-} BuiltinRule)
    ,userRules :: !(TMap.Map UserRuleVersioned)
    }

instance Semigroup SRules where
    (SRules x1 x2 x3) <> (SRules y1 y2 y3) = SRules (mappend x1 y1) (Map.unionWithKey f x2 y2) (TMap.unionWith (<>) x3 y3)
        where f k a b = throwImpure $ errorRuleDefinedMultipleTimes k [builtinLocation a, builtinLocation b]

instance Monoid SRules where
    mempty = SRules mempty Map.empty TMap.empty
    mappend = (<>)

instance Semigroup a => Semigroup (Rules a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Rules a) where
    mempty = return mempty
    mappend = (<>)


-- | Add a user rule. In general these should be specialised to the type expected by a builtin rule.
--   The user rules can be retrieved by 'getUserRuleList'.
addUserRule :: Typeable a => a -> Rules ()
addUserRule r = newRules mempty{userRules = TMap.singleton $ UserRuleVersioned False $ UserRule r}

-- | A suitable 'BuiltinLint' that always succeeds.
noLint :: BuiltinLint key value
noLint _ _ = return Nothing

-- | A suitable 'BuiltinIdentity' that always fails with a runtime error, incompatible with 'shakeShare'.
--   Use this function if you don't care about 'shakeShare', or if your rule provides a dependency that can
--   never be cached (in which case you should also call 'Development.Shake.historyDisable').
noIdentity :: Typeable key => BuiltinIdentity key value
noIdentity k _ = throwImpure $ errorStructured
    "Key type does not support BuiltinIdentity, so does not work with 'shakeShare'"
    [("Key type", Just $ show (typeOf k))] []


type family RuleResult key -- = value

-- | Define a builtin rule, passing the functions to run in the right circumstances.
--   The @key@ and @value@ types will be what is used by 'Development.Shake.apply'.
--   As a start, you can use 'noLint' and 'noIdentity' as the first two functions,
--   but are required to supply a suitable 'BuiltinRun'.
--
--   Raises an error if any other rule exists at this type.
addBuiltinRule
    :: (RuleResult key ~ value, ShakeValue key, ShakeValue value, Partial)
    => BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRule = withFrozenCallStack $ addBuiltinRuleInternal $ BinaryOp
    (putEx . Bin.toLazyByteString . execPut . put)
    (runGet get . LBS.fromChunks . return)

addBuiltinRuleEx
    :: (RuleResult key ~ value, ShakeValue key, BinaryEx key, Typeable value, NFData value, Show value, Partial)
    => BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRuleEx = addBuiltinRuleInternal $ BinaryOp putEx getEx


-- | Unexpected version of 'addBuiltinRule', which also lets me set the 'BinaryOp'.
addBuiltinRuleInternal
    :: (RuleResult key ~ value, ShakeValue key, Typeable value, NFData value, Show value, Partial)
    => BinaryOp key -> BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRuleInternal binary lint check (run :: BuiltinRun key value) = do
    let k = Proxy :: Proxy key
    let lint_ k v = lint (fromKey k) (fromValue v)
    let check_ k v = check (fromKey k) (fromValue v)
    let run_ k v b = fmap newValue <$> run (fromKey k) v b
    let binary_ = BinaryOp (putOp binary . fromKey) (newKey . getOp binary)
    newRules mempty{builtinRules = Map.singleton (typeRep k) $ BuiltinRule lint_ check_ run_ binary_ (Version 0) callStackTop}


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
priority d = modifyRules $ \s -> s{userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned b $ Priority d x) $ userRules s}


-- | Indicate that the nested rules have a given version. If you change the semantics of the rule then updating (or adding)
--   a version will cause the rule to rebuild in some circumstances.
--
-- @
-- 'versioned' 1 $ \"hello.*\" %> \\out ->
--     'writeFile'' out \"Writes v1 now\" -- previously wrote out v0
-- @
--
--   You should only use 'versioned' to track changes in the build source, for standard runtime dependencies you should use
--   other mechanisms, e.g. 'Development.Shake.addOracle'.
versioned :: Int -> Rules () -> Rules ()
versioned v = modifyRules $ \s -> s
    {userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned True $ Versioned (Version v) x) $ userRules s
    ,builtinRules = Map.map (\b -> b{builtinVersion = Version v}) $ builtinRules s
    }


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
alternatives = modifyRules $ \r -> r{userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned b $ Alternative x) $ userRules r}


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
action a = newRules mempty{actions=newListBuilder $ void a}


-- | Remove all actions specified in a set of rules, usually used for implementing
--   command line specification of what to build.
withoutActions :: Rules () -> Rules ()
withoutActions = modifyRules $ \x -> x{actions=mempty}
