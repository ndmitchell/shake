{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Oracle(
    addOracle, getOracle
    ) where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Hashable
import Data.Maybe
import Data.Typeable

import Development.Shake.Core


newtype Question = Question [String]
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype Answer = Answer [String]
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)


instance Rule Question Answer where
    validStored _ _ = return False


-- | Add extra information which your build should depend on. For example:
--
-- > addOracle ["ghc"] ["7.2.1"]
-- > addOracle ["ghc-pkg","shake"] ["1.0"]
--
--   If a rule depends on the GHC version, it can then use @'getOracle' ["ghc"]@, and
--   if the GHC version changes, the rule will rebuild.
addOracle :: [String] -> [String] -> Rules ()
addOracle question answer = rule $ \(Question q) ->
    if q == question then Just $ return $ Answer answer else Nothing


-- | Get information previously added with 'addOracle'.
getOracle :: [String] -> Action [String]
getOracle question = do Answer answer <- apply1 $ Question question; return answer
