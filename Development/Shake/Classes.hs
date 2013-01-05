{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

-- | This module reexports the six necessary type classes that every 'Rule' type must support.
--   You can use this module to define new rules without depending on the @binary@, @deepseq@ and @hashable@ packages.
module Development.Shake.Classes(
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue,
#endif
    Show,Typeable,Eq,Hashable,Binary,NFData
    ) where

import Data.Hashable
import Data.Typeable
import Data.Binary
import Control.DeepSeq


#if __GLASGOW_HASKELL__ >= 704
-- | Define an alias for the six type classes required for things involved in Shake 'Development.Shake.Rule's.
--   This alias is only available in GHC 7.4 and above, and requires the @ConstraintKinds@ extension.
--
--   To define your own values meeting the necessary constraints it is convenient to use the extensions
--   @GeneralizedNewtypeDeriving@ and @DeriveDataTypeable@ to write:
--
-- > newtype MyType = MyType (String, Bool) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)
#endif
