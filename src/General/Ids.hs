{-# LANGUAGE RecordWildCards, BangPatterns, GADTs #-}

module General.Ids(
    Ids, Id,
    empty, insert, lookup,
    null, size, sizeUpperBound,
    toList
    ) where

import Data.IORef.Extra
import Data.Primitive.Array
import General.Intern(Id(..))
import Control.Monad
import Data.Maybe
import Prelude hiding (lookup, null)
import GHC.Exts hiding (toList)


data Ids a = Ids (IORef (S a))

data S a = S
    {capacity :: {-# UNPACK #-} !Int -- ^ Number of entries in values, initially 0
    ,used :: {-# UNPACK #-} !Int -- ^ Capacity that has been used, assuming no gaps from index 0, initially 0
    ,values :: {-# UNPACK #-} !(MutableArray RealWorld (Maybe a))
    }


empty :: IO (Ids a)
empty = do
    -- important to start at capacity == 0 so I can implement null cheaply
    let capacity = 0
    let used = 0
    values <- newArray capacity Nothing
    Ids <$> newIORef S{..}


sizeUpperBound :: Ids a -> IO Int
sizeUpperBound (Ids ref) = do
    S{..} <- readIORef ref
    return used


size :: Ids a -> IO Int
size (Ids ref) = do
    S{..} <- readIORef ref
    let go !acc i
            | i < 0 = return acc
            | otherwise = do
                v <- readArray values i
                if isJust v then go (acc+1) (i-1) else go acc (i-1)
    go 0 (used-1)


toList :: Ids a -> IO [(Id, a)]
toList (Ids ref) = do
    S{..} <- readIORef ref
    let go i
            | i >= used = return []
            | otherwise = do
                v <- readArray values i
                case v of
                    Nothing -> go $ i+1
                    Just v -> do
                        xs <- go $ i+1
                        return $ (Id $ fromIntegral i, v) : xs
    go 0


null :: Ids a -> IO Bool
null ids = (== 0) <$> sizeUpperBound ids


insert :: Id -> a -> Ids a -> IO ()
insert (Id i) v (Ids ref) = do
    S{..} <- readIORef ref
    let ii = fromIntegral i
    if ii < capacity then do
        writeArray values ii $ Just v
        when (ii >= used) $ writeIORef' ref S{used=ii+1,..}
     else do
        c2 <- return $ max (capacity * 2) (ii + 10000)
        v2 <- newArray c2 Nothing
        copyMutableArray v2 0 values 0 capacity
        writeArray v2 ii $ Just v
        writeIORef' ref $ S c2 (ii+1) v2

lookup :: Id -> Ids a -> IO (Maybe a)
lookup (Id i) (Ids ref) = do
    S{..} <- readIORef ref
    let ii = fromIntegral i
    if ii < capacity then
        readArray values ii
     else
        return Nothing
