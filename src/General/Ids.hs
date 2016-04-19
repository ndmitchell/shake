{-# LANGUAGE RecordWildCards #-}

module General.Ids(
    Ids, Id,
    empty, insert, lookup
    ) where

import Data.IORef.Extra
import Data.Primitive.Array
import General.Intern(Id(..))
import Prelude hiding (lookup)
import GHC.Exts


data Ids a = Ids (IORef (S a))

data S a = S
    {capacity :: {-# UNPACK #-} !Int
    ,values :: {-# UNPACK #-} !(MutableArray RealWorld (Maybe a))
    }

empty :: IO (Ids a)
empty = do
    let capacity = 10000
    values <- newArray capacity Nothing
    Ids <$> newIORef S{..}

insert :: Id -> a -> Ids a -> IO ()
insert (Id i) v (Ids ref) = do
    S{..} <- readIORef ref
    let ii = fromIntegral i
    if ii < capacity then
        writeArray values ii $ Just v
     else do
        c2 <- return $ max (capacity * 2) (ii + 1000)
        v2 <- newArray capacity Nothing
        copyMutableArray v2 0 values 0 capacity
        writeArray values ii $ Just v
        writeIORef' ref $ S c2 v2

lookup :: Id -> Ids a -> IO (Maybe a)
lookup (Id i) (Ids ref) = do
    S{..} <- readIORef ref
    let ii = fromIntegral i
    if ii < capacity then
        readArray values ii
     else
        return Nothing
