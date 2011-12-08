{-# LANGUAGE CPP #-}

{- |
This module just contains the hash function on TypeRep. In future versions
of the hashable package it is available in Data.Hashable.
-}
module Development.Shake.TypeHash() where

import Data.Hashable

#if __GLASGOW_HASKELL__ < 702

import Data.Typeable
import System.IO.Unsafe

typeHash x = unsafePerformIO $ typeRepKey x

#else

import GHC.Fingerprint.Type(Fingerprint(..))
import Data.Typeable.Internal(TypeRep(..))

typeHash (TypeRep (Fingerprint x _) _ _) = fromIntegral x

#endif

typeHash :: TypeRep -> Int

instance Hashable TypeRep where
    hash = typeHash
