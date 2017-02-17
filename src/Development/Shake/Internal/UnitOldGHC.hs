{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
module Development.Shake.Internal.UnitOldGHC (Unit) where

class Unit a
instance Unit b => Unit (a -> b)
instance a ~ () => Unit (m a)
