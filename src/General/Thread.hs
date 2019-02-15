{-# LANGUAGE ScopedTypeVariables #-}

-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Thread(
    withThreadsBoth,
    allocateThread
    ) where

import General.Cleanup
import Control.Concurrent.Extra
import Control.Exception
import General.Extra
import Control.Monad.Extra


-- Run both actions. If either throws an exception, both threads
-- are killed and an exception reraised.
-- Not called much, so simplicity over performance (2 threads).
withThreadsBoth :: IO a -> IO b -> IO (a, b)
withThreadsBoth act1 act2 = do
    bar1 <- newBarrier
    bar2 <- newBarrier
    parent <- myThreadId
    ignore <- newVar False
    mask $ \unmask -> do
        t1 <- forkIOWithUnmask $ \unmask -> do
            res1 :: Either SomeException a <- try $ unmask act1
            unlessM (readVar ignore) $ whenLeft res1 $ throwTo parent
            signalBarrier bar1 res1
        t2 <- forkIOWithUnmask $ \unmask -> do
            res2 :: Either SomeException b <- try $ unmask act2
            unlessM (readVar ignore) $ whenLeft res2 $ throwTo parent
            signalBarrier bar2 res2
        res :: Either SomeException (a,b) <- try $ unmask $ do
            Right v1 <- waitBarrier bar1
            Right v2 <- waitBarrier bar2
            return (v1,v2)
        writeVar ignore True
        killThread t1
        forkIO $ killThread t2
        waitBarrier bar1
        waitBarrier bar2
        either throwIO return res


-- | Run the given action in a separate thread.
--   On cleanup, the thread will be killed before continuing.
--   If the action raises an exception it will be rethrown on the parent thread.
allocateThread :: Cleanup -> IO () -> IO ()
allocateThread cleanup act = do
    bar <- newBarrier
    parent <- myThreadId
    ignore <- newVar False
    void $ allocate cleanup
        (mask_ $ forkIOWithUnmask $ \unmask -> do
            res :: Either SomeException () <- try $ unmask act
            unlessM (readVar ignore) $ whenLeft res $ throwTo parent
            signalBarrier bar ()
        )
        (\t -> do writeVar ignore True; killThread t; waitBarrier bar)
