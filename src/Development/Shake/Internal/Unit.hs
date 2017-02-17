{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
module Development.Shake.Internal.Unit (Unit) where

#if __GLASGOW_HASKELL__ >= 710
class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
#else
class Unit a
instance Unit b => Unit (a -> b)
instance a ~ () => Unit (m a)
#endif

