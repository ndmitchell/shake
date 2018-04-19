{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Development.Shake.Internal.Rules.Oracle(
    addOracle, addOracleCache, addOracleHash, askOracle
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
import General.Binary
import Control.Applicative
import Prelude


-- Use short type names, since the names appear in the Haddock, and are too long if they are in full
newtype OracleQ question = OracleQ question
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype OracleA answer = OracleA answer
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult (OracleQ a) = OracleA (RuleResult a)

data Mode = Norm | Cache | Hash deriving Eq

addOracleMode :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => Mode -> (q -> Action a) -> Rules (q -> Action a)
addOracleMode mode act = do
        -- rebuild is automatic for oracles, skip just means we don't rebuild
        opts <- getShakeOptionsRules
        let skip = shakeRebuildApply opts "" == RebuildLater

        addBuiltinRule noLint $ \(OracleQ q) old changed -> case old of
            Just old | (mode /= Hash && skip) || (mode == Cache && not changed) ->
                return $ RunResult ChangedNothing old $ decode' old
            _ -> do
                new <- OracleA <$> act q
                let newHash = encodeHash new
                return $
                    if mode == Hash then RunResult
                        (if old == Just newHash then ChangedRecomputeSame else ChangedRecomputeDiff)
                        newHash
                        new
                    else RunResult
                        (if fmap decode' old == Just new then ChangedRecomputeSame else ChangedRecomputeDiff)
                        (encode' new)
                        new
        return askOracle
    where
        encodeHash :: Hashable a => a -> BS.ByteString
        encodeHash = runBuilder . putEx . hash

        encode' :: Binary a => a -> BS.ByteString
        encode' = BS.concat . LBS.toChunks . encode

        decode' :: Binary a => BS.ByteString -> a
        decode' = decode . LBS.fromChunks . return


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
addOracle = addOracleMode Norm


-- | An alternative to to 'addOracle' that relies on the 'hash' function providing a perfect equality,
--   doesn't support @--skip@, but requires less storage.
addOracleHash :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => (q -> Action a) -> Rules (q -> Action a)
addOracleHash = addOracleMode Hash

-- | A combination of 'addOracle' and 'newCache' - an action that only runs when its dependencies change,
--   whose result is stored in the database.
--
-- * Does the information need recomputing every time? e.g. looking up stuff in the environment?
--   If so, use 'addOracle' instead.
--
-- * Is the action mostly deserisalising some file? If so, use 'newCache'.
--
-- * Is the operation expensive computation from other results? If so, use 'addOracleCache'.
--
--   An alternative to using 'addOracleCache' is introducing an intermediate file containing the result,
--   which requires less storage in the Shake database and can be inspected by existing file-system viewing
--   tools.
addOracleCache ::(RuleResult q ~ a, ShakeValue q, ShakeValue a) => (q -> Action a) -> Rules (q -> Action a)
addOracleCache = addOracleMode Cache


-- | Get information previously added with 'addOracle' or 'addOracleCache'.
--   The question/answer types must match those provided previously.
askOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Action a
askOracle question = do OracleA answer <- apply1 $ OracleQ question; return answer
