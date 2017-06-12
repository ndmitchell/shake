{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Core.Rules(
    Rules, runRules,
    RuleResult, addBuiltinRule, addBuiltinRuleEx, noLint,
    getShakeOptionsRules, userRuleMatch,
    getUserRules, addUserRule, alternatives, priority,
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
import Data.Binary
import General.Binary
import Data.Typeable.Extra
import Data.Function
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import System.IO.Extra
import System.IO.Unsafe
import Data.Monoid
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

-- | Get the 'UserRule' value at a given type. This 'UserRule' will capture
--   all rules added, along with things such as 'priority' and 'alternatives'.
getUserRules :: Typeable a => Action (UserRule a)
getUserRules = f where
    f :: forall a . Typeable a => Action (UserRule a)
    f = do
        userRules <- Action $ getsRO globalUserRules
        return $ case Map.lookup (typeRep (Proxy :: Proxy a)) userRules of
            Nothing -> Unordered []
            Just (UserRule_ r) -> fromJust $ cast r


-- | Get the 'ShakeOptions' that were used.
getShakeOptionsRules :: Rules ShakeOptions
getShakeOptionsRules = Rules $ lift ask

-- | Give a 'UserRule', and a function that tests a given rule, return the most important values
--   that match. In most cases the caller will raise an error if the rule matching returns anything
--   other than a singleton.
userRuleMatch :: UserRule a -> (a -> Maybe b) -> [b]
userRuleMatch u test = head $ (map snd $ reverse $ groupSort $ f Nothing $ fmap test u) ++ [[]]
    where
        f :: Maybe Double -> UserRule (Maybe a) -> [(Double,a)]
        f p (UserRule x) = maybe [] (\x -> [(fromMaybe 1 p,x)]) x
        f p (Unordered xs) = concatMap (f p) xs
        f p (Priority p2 x) = f (Just $ fromMaybe p2 p) x
        f p (Alternative x) = case f p x of
            [] -> []
            -- a bit weird to use the max priority but the first value
            -- but that's what the current implementation does...
            xs -> [(maximum $ map fst xs, snd $ head xs)]


-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'.
--   Rules are combined with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
--   To define your own custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (WriterT SRules (ReaderT ShakeOptions IO) a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newRules :: SRules -> Rules ()
newRules = Rules . tell

modifyRules :: (SRules -> SRules) -> Rules () -> Rules ()
modifyRules f (Rules r) = Rules $ censor f r

runRules :: ShakeOptions -> Rules () -> IO ([Action ()], Map.HashMap TypeRep BuiltinRule, Map.HashMap TypeRep UserRule_)
runRules opts (Rules r) = do
    SRules{..} <- runReaderT (execWriterT r) opts
    return (runListBuilder actions, builtinRules, userRules)

data SRules = SRules
    {actions :: !(ListBuilder (Action ()))
    ,builtinRules :: !(Map.HashMap TypeRep{-k-} BuiltinRule)
    ,userRules :: !(Map.HashMap TypeRep{-k-} UserRule_)
    }

instance Monoid SRules where
    mempty = SRules mempty Map.empty Map.empty
    mappend (SRules x1 x2 x3) (SRules y1 y2 y3) = SRules (mappend x1 y1) (Map.unionWithKey f x2 y2) (Map.unionWith g x3 y3)
        where
            f k _ _ = unsafePerformIO $ errorRuleDefinedMultipleTimes k
            g (UserRule_ x) (UserRule_ y) = UserRule_ $ Unordered $ fromUnordered x ++ fromUnordered (fromJust $ cast y)

            fromUnordered (Unordered xs) = xs
            fromUnordered x = [x]


instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend = liftA2 mappend


-- | Add a value of type 'UserRule'.
addUserRule :: Typeable a => a -> Rules ()
addUserRule r = newRules mempty{userRules = Map.singleton (typeOf r) $ UserRule_ $ UserRule r}

-- | A suitable 'BuiltinLint' that always succeeds.
noLint :: BuiltinLint key value
noLint _ _ = return Nothing

type family RuleResult key -- = value

-- | Add a builtin rule, comprising of a lint rule and an action. Each builtin rule must be identified by
--   a unique key.
addBuiltinRule :: (RuleResult key ~ value, ShakeValue key, ShakeValue value) => BuiltinLint key value -> BuiltinRun key value -> Rules ()
addBuiltinRule = addBuiltinRuleEx $ BinaryOp
    (putEx . Bin.toLazyByteString . execPut . put)
    (runGet get . LBS.fromChunks . return)


-- | Initial version of 'addBuiltinRule', which also lets me set the 'BinaryOp'.
addBuiltinRuleEx :: (RuleResult key ~ value, ShakeValue key, ShakeValue value) => BinaryOp key -> BuiltinLint key value -> BuiltinRun key value -> Rules ()
addBuiltinRuleEx binary lint (run :: BuiltinRun key value) = do
    let k = Proxy :: Proxy key
        v = Proxy :: Proxy value
    let run_ k v b = fmap newValue <$> run (fromKey k) v b
    let lint_ k v = lint (fromKey k) (fromValue v)
    let binary_ = BinaryOp (putOp binary . fromKey) (newKey . getOp binary)
    newRules mempty{builtinRules = Map.singleton (typeRep k) $ BuiltinRule run_ lint_ (typeRep v) binary_}


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
priority d = modifyRules $ \s -> s{userRules = Map.map f $ userRules s}
    where f (UserRule_ s) = UserRule_ $ Priority d s


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
    where f (UserRule_ s) = UserRule_ $ Alternative s


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
