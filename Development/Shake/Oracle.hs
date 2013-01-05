{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
-- Allows the user to violate the functional dependency, but it has a runtime check so still safe
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Development.Shake.Oracle(
    addOracle, askOracle, askOracleWith
    ) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Typeable

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


-- | Add extra information which your build should depend on. For example:
--
-- @
--newtype GhcVersion = GhcVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--'addOracle' $ \GhcVersion -> return \"7.2.1\"
-- @
--
--   If a rule depends on the GHC version, it can then use @'askOracle' GhcVersion@, and
--   if the GHC version changes, the rule will rebuild. We use a @newtype@ around @()@ to
--   allow the use of @GeneralizedNewtypeDeriving@. It is common for the value returned
--   by 'askOracle' to be ignored, in which case 'askOracleWith' may help avoid ambiguous type
--   messages -- although a wrapper function with an explicit type is encouraged.
--   To import all the type classes required see "Development.Shake.Classes".
--
--   We require that each type of @question@ map to exactly one type of @answer@,
--   otherwise a runtime error will be raised.
--
--   Actions passed to 'addOracle' will be run in every build they are required,
--   but if their value does not change they will not invalidate any rules depending on them.
--   To get a similar behaviour using files, see 'Development.Shake.alwaysRerun'.
--
--   As an example, consider tracking package versions installed with GHC:
--
-- @
--newtype GhcPkgList = GhcPkgList () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--newtype GhcPkgVersion = GhcPkgVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--
--do
--    'addOracle' $ \\GhcPkgList{} -> do
--        (out,_) <- 'systemOutput' \"ghc-pkg\" [\"list\",\"--simple-output\"]
--        return [(reverse b, reverse a) | x <- words out, let (a,_:b) = break (== \'-\') $ reverse x]
--    let getPkgList = 'askOracleWith' (GhcPkgList ()) [(\"\",\"\")] 
--    --
--    'addOracle' $ \\(GhcPkgVersion pkg) -> do
--        pkgs <- getPkgList
--        return $ lookup pkg pkgs
--    let getPkgVersion pkg = 'askOracleWith' (GhcPkgVersion pkg) (Just \"\")
-- @
--
--   Using these definitions, any rule depending on the version of @shake@
--   should call @getPkgVersion "shake"@ to rebuild when @shake@ is upgraded.
addOracle :: (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => (q -> Action a) -> Rules ()
addOracle act = rule $ \(OracleQ q) -> Just $ fmap OracleA $ act q


-- | Get information previously added with 'addOracle', the @question@/@answer@ types must match those provided
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

-- | Get information previously added with 'addOracle'. The second argument is unused, but can
--   be useful to avoid ambiguous type error messages.
askOracleWith :: (
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue q, ShakeValue a
#else
    Show q, Typeable q, Eq q, Hashable q, Binary q, NFData q,
    Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a
#endif
    ) => q -> a -> Action a
askOracleWith question _ = askOracle question
