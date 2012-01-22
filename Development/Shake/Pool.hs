
-- | Thread pool implementation.
module Development.Shake.Pool(Pool, addPool, blockPool, runPool) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef


data Pool = Pool (IORef (Maybe SomeException)) (MVar ()) (MVar Int)


-- | Add a new task to the pool
addPool :: Pool -> IO a -> IO ()
addPool (Pool err done count) act = do
    modifyMVar_ count $ \count -> do
        when (count == 0) $ takeMVar done
        return $ count + 1
    forkIO $ do
        res <- try act
        modifyMVar_ count $ \count -> do
            when (count == 1) $ putMVar done ()
            case res of
                Left e -> writeIORef err $ Just e
                _ -> return ()
            return $ count - 1
    return ()


-- | A blocking action is being run while on the pool, yeild your thread.
blockPool :: IO a -> IO a
blockPool act = act


-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool n act = do
    count <- newMVar 0
    done <- newMVar ()
    err <- newIORef Nothing
    act $ Pool err done count
    takeMVar done
    e <- readIORef err
    case e of
        Nothing -> return ()
        Just err -> throw err

