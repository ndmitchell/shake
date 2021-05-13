{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Rules.Rerun(
    defaultRuleRerun, AlwaysRerunQ(..)
    ) where

import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Types
import Development.Shake.Classes
import qualified Data.ByteString as BS
import General.Binary


newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)
instance Show AlwaysRerunQ where show _ = "alwaysRerun"

type instance RuleResult AlwaysRerunQ = ()



defaultRuleRerun :: Rules ()
defaultRuleRerun =
    addBuiltinRuleEx noLint noIdentity $
        \AlwaysRerunQ{} _ _ -> pure $ RunResult ChangedRecomputeDiff BS.empty ()
