{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Development.Shake.Internal.UnitNewGHC (Unit) where

class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
