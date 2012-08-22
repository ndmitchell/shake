{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.Oracle(
    addOracle, askOracle
    ) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Typeable

import Development.Shake.Core


newtype Question = Question [String]
    deriving (Typeable,Eq,Hashable,Binary,NFData)
newtype Answer = Answer [String]
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

instance Show Question where
    show (Question xs) = "Oracle " ++ unwords xs

instance Rule Question Answer where
    validStored _ _ = return False


-- | Add extra information which your build should depend on. For example:
--
-- > addOracle ["ghc"] $ return ["7.2.1"]
-- > addOracle ["ghc-pkg","shake"] $ return ["1.0"]
--
--   If a rule depends on the GHC version, it can then use @'askOracle' [\"ghc\"]@, and
--   if the GHC version changes, the rule will rebuild. It is common for the value returned
--   by 'askOracle' to be ignored.
--
--   The Oracle maps questions of @[String]@ and answers of @[String]@. This type is a
--   compromise. Questions will often be the singleton list, but allowing a list of strings
--   gives more flexibility for hierarchical schemes and grouping - i.e. to have
--   @ghc-pkg shake@, @ghc-pkg base@ etc. The answers are often singleton lists, but
--   sometimes are used as sets - for example the list of packages returned by @ghc-pkg@.
--
--   Actions passed to 'addOracle' will be run in every Shake execution they are required,
--   their value will not be kept between runs. To get a similar behaviour using files, see
--   'Development.Shake.alwaysRerun'.
addOracle :: [String] -> Action [String] -> Rules ()
addOracle question act = rule $ \(Question q) ->
    if q == question then Just $ fmap Answer act else Nothing


-- | Get information previously added with 'addOracle'.
askOracle :: [String] -> Action [String]
askOracle question = do Answer answer <- apply1 $ Question question; return answer
