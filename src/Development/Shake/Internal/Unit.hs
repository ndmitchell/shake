{-# LANGUAGE CPP #-}
module Development.Shake.Internal.Unit (Unit (..)) where
#if __GLASGOW_HASKELL__ >= 710
import Development.Shake.Internal.UnitNewGHC
#else
import Development.Shake.Internal.UnitOldGHC
#endif

