{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
-- Allows the user to violate the functional dependency, but it has a runtime check so still safe
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Development.Shake.Rules.Oracle(
    addOracle, askOracle, askOracleWith
    ) where

import Development.Shake.Core
import Development.Shake.Classes


-- Use should type names, since the names appear in the Haddock, and are too long if they are in full
newtype OracleQ question = OracleQ question
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OracleA answer = OracleA answer
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

instance (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => Rule (OracleQ q) (OracleA a) where
    storedValue _ = return Nothing


-- | Add extra information which rules can depend on.
--   An oracle is a function from a question type @q@, to an answer type @a@.
--   As an example, we can define an oracle allowing you to depend on the current version of GHC:
--
-- @
-- newtype GhcVersion = GhcVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
-- rules = do
--     'addOracle' $ \\(GhcVersion _) -> fmap 'Development.Shake.fromStdout' $ 'Development.Shake.cmd' \"ghc --numeric-version\"
--     ... rules ...
-- @
--
--   If a rule calls @'askOracle' (GhcVersion ())@, that rule will be rerun whenever the GHC version changes.
--   Some notes:
--
-- * We define @GhcVersion@ with a @newtype@ around @()@, allowing the use of @GeneralizedNewtypeDeriving@.
--   All the necessary type classes are exported from "Development.Shake.Classes".
--
-- * Each call to 'addOracle' must use a different type of question.
--
-- * Actions passed to 'addOracle' will be run in every build they are required,
--   but if their value does not change they will not invalidate any rules depending on them.
--   To get a similar behaviour using data stored in files, see 'Development.Shake.alwaysRerun'.
--
-- * If the value returned by 'askOracle' is ignored then 'askOracleWith' may help avoid ambiguous type messages.
--   Alternatively, use the result of 'addOracle', which is 'askOracle' restricted to the correct type.
--
--   As a more complex example, consider tracking Haskell package versions:
--
-- @
--newtype GhcPkgList = GhcPkgList () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--newtype GhcPkgVersion = GhcPkgVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--
--rules = do
--    getPkgList \<- 'addOracle' $ \\GhcPkgList{} -> do
--        Stdout out <- 'Development.Shake.cmd' \"ghc-pkg list --simple-output\"
--        return [(reverse b, reverse a) | x <- words out, let (a,_:b) = break (== \'-\') $ reverse x]
--    --
--    getPkgVersion \<- 'addOracle' $ \\(GhcPkgVersion pkg) -> do
--        pkgs <- getPkgList $ GhcPkgList ()
--        return $ lookup pkg pkgs
--    --
--    \"myrule\" *> \\_ -> do
--        getPkgVersion $ GhcPkgVersion \"shake\"
--        ... rule using the shake version ...
-- @
--
--   Using these definitions, any rule depending on the version of @shake@
--   should call @getPkgVersion $ GhcPkgVersion \"shake\"@ to rebuild when @shake@ is upgraded.
addOracle :: (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => (q -> Action a) -> Rules (q -> Action a)
addOracle act = do
    rule $ \(OracleQ q) -> Just $ fmap OracleA $ act q
    return askOracle


-- | Get information previously added with 'addOracle'. The question/answer types must match those provided
--   to 'addOracle'.
askOracle :: (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => q -> Action a
askOracle question = do OracleA answer <- apply1 $ OracleQ question; return answer

-- | Get information previously added with 'addOracle'. The second argument is not used, but can
--   be useful to fix the answer type, avoiding ambiguous type error messages.
askOracleWith :: (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => q -> a -> Action a
askOracleWith question _ = askOracle question
