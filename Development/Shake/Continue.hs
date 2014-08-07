
module Development.Shake.Continue(
    Continue(..),
    Fence, newFence, signalFence, waitFence, testFence,
    ) where

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.IORef


newtype Continue a = Continue {runContinue :: Either SomeException a -> IO ()}


-- | Like a barrier, but based on callbacks
newtype Fence a = Fence (IORef (Either [Continue a] (Either SomeException a)))
instance Show (Fence a) where show _ = "Fence"


newFence :: IO (Fence a)
newFence = Fence <$> newIORef (Left [])

signalFence :: Fence a -> Continue a
signalFence (Fence ref) = Continue $ \v -> join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Right v, mapM_ (($ v) . runContinue) $ reverse queue)
    Right v -> error "Shake internal error, signalFence called twice on one Fence"

waitFence :: Fence a -> Continue a -> IO ()
waitFence (Fence ref) call = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Left (call:queue), return ())
    Right v -> (Right v, runContinue call v)

testFence :: Fence a -> IO (Maybe (Either SomeException a))
testFence (Fence x) = either (const Nothing) Just <$> readIORef x
