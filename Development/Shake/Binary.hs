{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Development.Shake.Binary(
    BinaryWith(..), module Data.Binary
    ) where

import Control.Monad
import Data.Binary

class BinaryWith ctx a where
    putWith :: ctx -> a -> Put
    getWith :: ctx -> Get a

instance (BinaryWith ctx a, BinaryWith ctx b) => BinaryWith ctx (a,b) where
    putWith ctx (a,b) = putWith ctx a >> putWith ctx b
    getWith ctx = do a <- getWith ctx; b <- getWith ctx; return (a,b)

instance BinaryWith ctx a => BinaryWith ctx [a] where
    putWith ctx xs = put (length xs) >> mapM_ (putWith ctx) xs
    getWith ctx = do n <- get; replicateM n $ getWith ctx
