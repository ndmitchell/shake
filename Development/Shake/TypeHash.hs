{-# LANGUAGE CPP #-}

{- |
This module just contains the hash function on TypeRep. Ideally would be moved
into Data.Hashable.
-}
module Development.Shake.TypeHash(
    typeHash
    ) where

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
