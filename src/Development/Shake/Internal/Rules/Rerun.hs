{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Internal.Rules.Rerun(
    defaultRuleRerun, alwaysRerun
    ) where

import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Types
import Development.Shake.Classes
import qualified Data.ByteString as BS
import General.Encoder


newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,Encoder,NFData)
instance Show AlwaysRerunQ where show _ = "alwaysRerun"

newtype AlwaysRerunA = AlwaysRerunA ()
    deriving (Typeable,Hashable,Binary,Encoder,NFData)
instance Show AlwaysRerunA where show _ = "<none>"
instance Eq AlwaysRerunA where a == b = False


-- | Always rerun the associated action. Useful for defining rules that query
--   the environment. For example:
--
-- @
-- \"ghcVersion.txt\" 'Development.Shake.%>' \\out -> do
--     'alwaysRerun'
--     'Development.Shake.Stdout' stdout <- 'Development.Shake.cmd' \"ghc --numeric-version\"
--     'Development.Shake.writeFileChanged' out stdout
-- @
--
--   In make, the @.PHONY@ attribute on file-producing rules has a similar effect.
--
--   Note that 'alwaysRerun' is applied when a rule is executed. Modifying an existing rule
--   to insert 'alwaysRerun' will /not/ cause that rule to rerun next time.
alwaysRerun :: Action ()
alwaysRerun = do AlwaysRerunA _ <- apply1 $ AlwaysRerunQ (); return ()

defaultRuleRerun :: Rules ()
defaultRuleRerun = do
    addBuiltinRule noLint $
        \AlwaysRerunQ{} _ _ -> return $ RunResult ChangedRecomputeDiff BS.empty $ AlwaysRerunA ()
