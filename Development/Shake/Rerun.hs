{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Rerun(
    defaultRuleRerun, alwaysRerun
    ) where

import Development.Shake.Core
import Development.Shake.Classes


newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,NFData)
instance Show AlwaysRerunQ where show _ = "AlwaysRerunQ"

newtype AlwaysRerunA = AlwaysRerunA ()
    deriving (Typeable,Hashable,Binary,NFData)
instance Show AlwaysRerunA where show _ = "AlwaysRerunA"
instance Eq AlwaysRerunA where a == b = False

instance Rule AlwaysRerunQ AlwaysRerunA where
    storedValue _ = return Nothing


-- | Always rerun the associated action. Useful for defining rules that query
--   the environment. For example:
--
-- @
-- \"ghcVersion.txt\" 'Development.Shake.*>' \\out -> do
--     'alwaysRerun'
--     (stdout,_) <- 'Development.Shake.systemOutput' \"ghc\" [\"--version\"]
--     'Development.Shake.writeFileChanged' out stdout
-- @
alwaysRerun :: Action ()
alwaysRerun = do AlwaysRerunA _ <- apply1 $ AlwaysRerunQ (); return ()

defaultRuleRerun :: Rules ()
defaultRuleRerun = defaultRule $ \AlwaysRerunQ{} -> Just $ return $ AlwaysRerunA()
