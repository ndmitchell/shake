{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Development.Shake.Internal.Rules.Oracle(
    addOracle, askOracle
    ) where

import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Options
import Development.Shake.Internal.Value
import Development.Shake.Classes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Control.Applicative
import Prelude


-- Use short type names, since the names appear in the Haddock, and are too long if they are in full
newtype OracleQ question = OracleQ question
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OracleA answer = OracleA answer
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult (OracleQ a) = OracleA (RuleResult a)


-- | Add extra information which rules can depend on.
--   An oracle is a function from a question type @q@, to an answer type @a@.
--   As an example, we can define an oracle allowing you to depend on the current version of GHC:
--
-- @
-- newtype GhcVersion = GhcVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
-- type instance RuleResult GhcVersion = String
-- rules = do
--     'addOracle' $ \\(GhcVersion _) -> fmap 'Development.Shake.fromStdout' $ 'Development.Shake.cmd' \"ghc --numeric-version\" :: Action String
--     ... rules ...
-- @
--
--   If a rule calls @'askOracle' (GhcVersion ())@, that rule will be rerun whenever the GHC version changes.
--   Some notes:
--
-- * We define @GhcVersion@ with a @newtype@ around @()@, allowing the use of @GeneralizedNewtypeDeriving@.
--   All the necessary type classes are exported from "Development.Shake.Classes".
--
-- * The @type instance@ requires the extension @TypeFamilies@.
--
-- * Each call to 'addOracle' must use a different type of question.
--
-- * Actions passed to 'addOracle' will be run in every build they are required, even if nothing else changes,
--   so be careful of slow actions.
--   If the result of an oracle does not change it will not invalidate any rules depending on it.
--   To always rerun files rules see 'Development.Shake.alwaysRerun'.
--
-- * If the value returned by 'askOracle' is ignored then 'askOracleWith' may help avoid ambiguous type messages.
--   Alternatively, use the result of 'addOracle', which is 'askOracle' restricted to the correct type.
--
--   As a more complex example, consider tracking Haskell package versions:
--
-- @
-- newtype GhcPkgList = GhcPkgList () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
-- type instance RuleResult GhcPkgList = [(String, String)]
-- newtype GhcPkgVersion = GhcPkgVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
-- type instance RuleResult GhcPkgVersion = Maybe String
--
-- rules = do
--     getPkgList \<- 'addOracle' $ \\GhcPkgList{} -> do
--         Stdout out <- 'Development.Shake.cmd' \"ghc-pkg list --simple-output\"
--         return [(reverse b, reverse a) | x <- words out, let (a,_:b) = break (== \'-\') $ reverse x]
--
--     getPkgVersion \<- 'addOracle' $ \\(GhcPkgVersion pkg) -> do
--         pkgs <- getPkgList $ GhcPkgList ()
--         return $ lookup pkg pkgs
--
--     \"myrule\" %> \\_ -> do
--         getPkgVersion $ GhcPkgVersion \"shake\"
--         ... rule using the shake version ...
-- @
--
--   Using these definitions, any rule depending on the version of @shake@
--   should call @getPkgVersion $ GhcPkgVersion \"shake\"@ to rebuild when @shake@ is upgraded.
addOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => (q -> Action a) -> Rules (q -> Action a)
addOracle act = do
        -- rebuild is automatic for oracles, skip just means we don't rebuild
        opts <- getShakeOptionsRules
        let skip = shakeRebuildApply opts "" == RebuildLater

        addBuiltinRule noLint $ \(OracleQ q) old _ -> case old of
            Just old | skip ->
                return $ RunResult ChangedNothing old $ decode' old
            _ -> do
                new <- OracleA <$> act q
                return $ RunResult
                    (if fmap decode' old == Just new then ChangedRecomputeSame else ChangedRecomputeDiff)
                    (encode' new)
                    new
        return askOracle
    where
        encode' :: Binary a => a -> BS.ByteString
        encode' = BS.concat . LBS.toChunks . encode

        decode' :: Binary a => BS.ByteString -> a
        decode' = decode . LBS.fromChunks . return


-- | Get information previously added with 'addOracle'. The question/answer types must match those provided
--   to 'addOracle'.
askOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Action a
askOracle question = do OracleA answer <- apply1 $ OracleQ question; return answer
