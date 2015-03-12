
module General.Concurrent(
    Fence, newFence, signalFence, waitFence, testFence,
    ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Prelude


---------------------------------------------------------------------
-- FENCE

-- | Like a barrier, but based on callbacks
newtype Fence a = Fence (IORef (Either [a -> IO ()] a))
instance Show (Fence a) where show _ = "Fence"

newFence :: IO (Fence a)
newFence = Fence <$> newIORef (Left [])

signalFence :: Fence a -> a -> IO ()
signalFence (Fence ref) v = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Right v, mapM_ ($ v) $ reverse queue)
    Right v -> error "Shake internal error, signalFence called twice on one Fence"

waitFence :: Fence a -> (a -> IO ()) -> IO ()
waitFence (Fence ref) call = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Left (call:queue), return ())
    Right v -> (Right v, call v)

testFence :: Fence a -> IO (Maybe a)
testFence (Fence x) = either (const Nothing) Just <$> readIORef x
