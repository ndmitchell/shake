
module Development.Shake.Continue(
    Fence, newFence, signalFence, waitFence, testFence,
    ) where

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.IORef


-- | Like a barrier, but based on callbacks
newtype Fence a = Fence (IORef (Either [Either SomeException a -> IO ()] (Either SomeException a)))
instance Show (Fence a) where show _ = "Fence"


newFence :: IO (Fence a)
newFence = Fence <$> newIORef (Left [])

signalFence :: Fence a -> Either SomeException a -> IO ()
signalFence (Fence ref) v = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Right v, mapM_ ($ v) $ reverse queue)
    Right v -> error "Shake internal error, signalFence called twice on one Fence"

waitFence :: Fence a -> (Either SomeException a -> IO ()) -> IO ()
waitFence (Fence ref) call = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Left (call:queue), return ())
    Right v -> (Right v, call v)

testFence :: Fence a -> IO (Maybe (Either SomeException a))
testFence (Fence x) = either (const Nothing) Just <$> readIORef x
