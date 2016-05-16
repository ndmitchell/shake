{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Rules.Rerun(
    defaultRuleRerun, alwaysRerun, outputCheck
    ) where

import Development.Shake.Core
import Development.Shake.Classes
import Development.Shake.Types

import qualified Data.ByteString.Lazy as LBS

newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,NFData,Show)

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
alwaysRerun = apply1 $ AlwaysRerunQ ()

defaultRuleRerun :: Rules ()
defaultRuleRerun = do
    simpleCheck (\OutputCheck{} -> fmap shakeOutputCheck getShakeOptions)
    addBuiltinRule $ \AlwaysRerunQ{} (_ :: Maybe ()) _ -> do
        return $ BuiltinResult
            { resultStoreB = LBS.empty
            , resultValueB = ()
            , ranDependsB = True
            , unchangedB = False
            }

newtype OutputCheck = OutputCheck ()
    deriving (Typeable,Eq,Hashable,Binary,NFData,Show)

-- | A tracking version of 'shakeOutputCheck' that will re-run all relevant rules when it changes.
outputCheck :: Action Bool
outputCheck = apply1 (OutputCheck ())
