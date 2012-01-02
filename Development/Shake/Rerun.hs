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
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype Dirty = Dirty ()
    deriving (Show,Typeable,Hashable,Binary,NFData)
instance Eq Dirty where a == b = False

instance Rule AlwaysRerun Dirty where
    validStored _ _ = return False


-- | Always rerun the associated action. Useful for defining rules that look something
--   up in the environment. For example:
--
-- > "ghcVersion.txt" *> \out -> do
-- >     alwaysRerun
-- >     (stdout,_) <- systemOutput' "ghc" "--version"
-- >     writeFile' out stdout
alwaysRerun :: Action ()
alwaysRerun = do Dirty _ <- apply1 $ AlwaysRerun (); return ()

defaultRuleRerun :: Rules ()
defaultRuleRerun = defaultRule $ \AlwaysRerun{} -> Just $ return $ Dirty ()
