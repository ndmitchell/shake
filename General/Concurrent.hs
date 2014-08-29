
module General.Concurrent(
    Lock, newLock, withLock, withLockTry,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    Fence, newFence, signalFence, waitFence, testFence,
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import General.Base


---------------------------------------------------------------------
-- LOCK

-- | Like an MVar, but has no value
newtype Lock = Lock (MVar ())
instance Show Lock where show _ = "Lock"

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const

withLockTry :: Lock -> IO a -> IO (Maybe a)
withLockTry (Lock m) act =
    mask $ \restore -> do
        a <- tryTakeMVar m
        case a of
            Nothing -> return Nothing
            Just _ -> restore (fmap Just act) `finally` putMVar m ()


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)
instance Show (Var a) where show _ = "Var"

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

withVar :: Var a -> (a -> IO b) -> IO b
withVar (Var x) f = withMVar x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once
newtype Barrier a = Barrier (MVar a)
instance Show (Barrier a) where show _ = "Barrier"

newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = void . tryPutMVar x

waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x

waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier x) = do
    res <- tryTakeMVar x
    whenJust res $ void . tryPutMVar x
    return res


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
