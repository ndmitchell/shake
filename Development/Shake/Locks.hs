
module Development.Shake.Locks(
    Var, newVar, readVar, modifyVar, modifyVar_,
    Barrier, newBarrier, releaseBarrier, waitBarrier, waitAnyBarrier
    ) where

import Control.Concurrent
import Control.Monad
import Data.IORef


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f


---------------------------------------------------------------------
-- BARRIER

-- Either Nothing to indicate it has been released already,
-- or Just the list of actions to run when released
newtype Barrier = Barrier (IORef (Maybe [IO ()]))

newBarrier :: IO Barrier
newBarrier = fmap Barrier $ newIORef $ Just []


releaseBarrier :: Barrier -> IO ()
releaseBarrier (Barrier v) = do
    xs <- atomicModifyIORef v $ \v -> (Nothing, v)
    sequence_ $ maybe [] reverse xs


waitBarrier :: Barrier -> IO ()
waitBarrier (Barrier v) = do
    i <- newEmptyMVar
    b <- atomicModifyIORef v $ \v -> case v of
        Nothing -> (Nothing, False)
        Just xs -> (Just $ putMVar i ():xs, True)
    when b $ takeMVar i


waitAnyBarrier :: [Barrier] -> IO ()
waitAnyBarrier bs = do
    i <- newEmptyMVar 
    ref <- newIORef True
    let f = do
            b <- atomicModifyIORef ref $ \x -> (False,x)
            when b $ putMVar i ()
    b <- fmap and $ forM bs $ \(Barrier v) ->
        atomicModifyIORef v $ \v -> case v of
            Nothing -> (Nothing, False)
            Just xs -> (Just $ f:xs, True)
    when b $ takeMVar i
