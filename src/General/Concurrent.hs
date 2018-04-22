
module General.Concurrent(
    Fence, newFence, signalFence, waitFence, testFence,
    exceptFence
    ) where

import Control.Monad
import Data.Maybe
import Data.Either.Extra
import Data.Functor
import Data.IORef
import Prelude


---------------------------------------------------------------------
-- FENCE

-- | Like a barrier, but based on callbacks
newtype Fence a = Fence (IORef (Either (a -> IO ()) a))
instance Show (Fence a) where show _ = "Fence"

newFence :: IO (Fence a)
newFence = Fence <$> newIORef (Left $ const $ return ())

signalFence :: Fence a -> a -> IO ()
signalFence (Fence ref) v = join $ atomicModifyIORef' ref $ \x -> case x of
    Left queue -> (Right v, queue v)
    Right v -> error "Shake internal error, signalFence called twice on one Fence"

waitFence :: Fence a -> (a -> IO ()) -> IO ()
waitFence (Fence ref) call = join $ atomicModifyIORef' ref $ \x -> case x of
    Left queue -> (Left (\a -> queue a >> call a), return ())
    Right v -> (Right v, call v)

testFence :: Fence a -> IO (Maybe a)
testFence (Fence x) = either (const Nothing) Just <$> readIORef x


---------------------------------------------------------------------
-- FENCE COMPOSITES

exceptFence :: [Fence (Either e r)] -> IO (Fence (Either e [r]))
exceptFence xs = do
    -- number of items still to complete, becomes negative after it has triggered
    todo <- newIORef $ length xs
    fence <- newFence

    forM_ xs $ \x -> waitFence x $ \res ->
        join $ atomicModifyIORef' todo $ \i -> case res of
            Left e | i >= 0 -> (-1, signalFence fence $ Left e)
            _ | i == 1 -> (-1, signalFence fence . Right =<< mapM (fmap (fromRight' . fromJust) . testFence) xs)
              | otherwise -> (i-1, return ())
    return fence
