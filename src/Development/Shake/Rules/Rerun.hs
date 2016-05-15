{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Rules.Rerun(
    defaultRuleRerun, alwaysRerun
    ) where

import Development.Shake.Core
import Development.Shake.Core2
import Development.Shake.Classes


newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,NFData,Show)

newtype AlwaysRerunA = AlwaysRerunA ()
    deriving (Typeable,Hashable,Binary,NFData,Show,Eq)

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
defaultRuleRerun = newBuiltinRule (typeOf (undefined :: AlwaysRerunQ)) (BuiltinRule
        { execute = \_ _ _ -> do
            let v = AlwaysRerunA ()
            return $ BuiltinResult
              { resultStoreB = encode v
              , resultValueB = toDyn v
              , dependsB = Nothing
              , changedB = True
              }
        })
