{-# LANGUAGE RecordWildCards #-}

module General.Ids(
    Ids, Id,
    empty, insert, lookup, null
    ) where

import Data.IORef.Extra
import Data.Primitive.Array
import General.Intern(Id(..))
import Control.Monad
import Data.Maybe
import Prelude hiding (lookup, null)
import GHC.Exts


data Ids a = Ids (IORef (S a))

data S a = S
    {capacity :: {-# UNPACK #-} !Int -- ^ Number of entries in values, initially 0
    ,values :: {-# UNPACK #-} !(MutableArray RealWorld (Maybe a))
    }


empty :: IO (Ids a)
empty = do
    -- important to start at capacity == 0 so I can implement null cheaply
    let capacity = 0
    values <- newArray capacity Nothing
    Ids <$> newIORef S{..}


null :: Ids a -> IO Bool
null (Ids ref) = do
    S{..} <- readIORef ref
    -- safe because of empty
    return $ capacity == 0


insert :: Id -> a -> Ids a -> IO ()
insert (Id i) v (Ids ref) = do
    S{..} <- readIORef ref
    let ii = fromIntegral i
    if ii < capacity then
        writeArray values ii $ Just v
     else do
        c2 <- return $ max (capacity * 2) (ii + 10000)
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
