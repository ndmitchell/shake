{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Rerun(
    defaultRuleRerun, alwaysRerun
    ) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Typeable

import Development.Shake.Core


newtype AlwaysRerun = AlwaysRerun ()
    deriving (Typeable,Eq,Hashable,Binary,NFData)
instance Show AlwaysRerun where show _ = "AlwaysRerun"

newtype Dirty = Dirty ()
    deriving (Typeable,Hashable,Binary,NFData)
instance Show Dirty where show _ = "Dirty"
instance Eq Dirty where a == b = False

instance Rule AlwaysRerun Dirty where
    validStored _ _ = return False


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
alwaysRerun = do Dirty _ <- apply1 $ AlwaysRerun (); return ()

defaultRuleRerun :: Rules ()
defaultRuleRerun = defaultRule $ \AlwaysRerun{} -> Just $ return $ Dirty ()
