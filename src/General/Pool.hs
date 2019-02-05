
-- | Thread pool implementation. The three names correspond to the following
--   priority levels (highest to lowest):
--
-- * 'addPoolException' - things that probably result in a build error,
--   so kick them off quickly.
--
-- * 'addPoolResume' - things that started, blocked, and may have open
--   resources in their closure.
--
-- * 'addPoolStart' - rules that haven't yet started.
--
-- * 'addPoolBatch' - rules that might batch if other rules start first.
module General.Pool(
    Pool, runPool,
    addPool, PoolPriority(..),
    increasePool, keepAlivePool
    ) where

import Control.Concurrent.Extra
import System.Time.Extra
import Control.Exception
import Control.Monad.Extra
import General.Timing
import General.Extra
import qualified General.Heap as Heap
import qualified Data.HashSet as Set
import Data.IORef.Extra
import System.Random


---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
If any worker throws an exception, must signal to all the other workers
-}

data Pool = Pool
    !(Var (Maybe S)) -- Current state, 'Nothing' to say we are aborting
    !(Barrier (Either SomeException S)) -- Barrier to signal that we are finished

data S = S
    {threads :: !(Set.HashSet ThreadId) -- IMPORTANT: Must be strict or we leak thread stacks
    ,threadsLimit :: {-# UNPACK #-} !Int -- user supplied thread limit, Set.size threads <= threadsLimit
    ,threadsCount :: {-# UNPACK #-} !Int -- Set.size threads, but in O(1)
    ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads (accounting only)
    ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through (accounting only)
    ,rand :: IO Int -- operation to give us the next random Int
    ,todo :: !(Heap.Heap (Heap.Entry (PoolPriority, Int) (IO ()))) -- operations waiting a thread
    }


emptyS :: Int -> Bool -> IO S
emptyS n deterministic = do
    rand <- if not deterministic then return randomIO else do
        ref <- newIORef 0
        -- no need to be thread-safe - if two threads race they were basically the same time anyway
        return $ do i <- readIORef ref; writeIORef' ref (i+1); return i
    return $ S Set.empty n 0 0 0 rand Heap.empty


worker :: Pool -> IO ()
worker pool@(Pool var _) = do
    let onVar act = modifyVar var $ maybe (return (Nothing, return ())) act
    join $ onVar $ \s ->return $ case Heap.uncons $ todo s of
        Nothing -> (Just s, return ())
        Just (Heap.Entry _ now, todo2) -> (Just s{todo = todo2}, now >> worker pool)

-- | Given a pool, and a function that breaks the S invariants, restore them
--   They are only allowed to touch threadsLimit or todo
step :: Pool -> (S -> IO S) -> IO ()
step pool@(Pool var done) op = do
    let onVar act = modifyVar_ var $ maybe (return Nothing) act
    onVar $ \s -> do
        s <- op s
        case Heap.uncons $ todo s of
            Just (Heap.Entry _ now, todo2) | threadsCount s < threadsLimit s -> do
                -- spawn a new worker
                t <- forkFinallyUnmasked (now >> worker pool) $ \res -> case res of
                    Left e -> onVar $ \s -> do
                        t <- myThreadId
                        mapM_ killThread $ Set.toList $ Set.delete t $ threads s
                        signalBarrier done $ Left e
                        return Nothing
                    Right _ -> do
                        t <- myThreadId
                        step pool $ \s -> return s{threads = Set.delete t $ threads s, threadsCount = threadsCount s - 1}
                return $ Just s{todo = todo2, threads = Set.insert t $ threads s, threadsCount = threadsCount s + 1
                               ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` (threadsCount s + 1)}
            Nothing | threadsCount s == 0 -> do
                signalBarrier done $ Right s
                return Nothing
            _ -> return $ Just s


-- | Add a new task to the pool. See the top of the module for the relative ordering
--   and semantics.
addPool :: PoolPriority -> Pool -> IO a -> IO ()
addPool priority pool act = step pool $ \s -> do
    i <- rand s
    return s{todo = Heap.insert (Heap.Entry (priority, i) $ void act) $ todo s}


data PoolPriority
    = PoolException
    | PoolResume
    | PoolStart
    | PoolBatch
    | PoolDeprioritize Double
      deriving (Eq,Ord)

-- | Temporarily increase the pool by 1 thread. Call the cleanup action to restore the value.
--   After calling cleanup you should requeue onto a new thread.
increasePool :: Pool -> IO (IO ())
increasePool pool = do
    step pool $ \s -> return s{threadsLimit = threadsLimit s + 1}
    return $ step pool $ \s -> return s{threadsLimit = threadsLimit s - 1}


-- | Make sure the pool cannot run out of tasks (and thus everything finishes) until after the cancel is called.
--   Ensures that a pool that will requeue in time doesn't go idle.
keepAlivePool :: Pool -> IO (IO ())
keepAlivePool pool = do
    bar <- newBarrier
    addPool PoolResume pool $ do
        cancel <- increasePool pool
        waitBarrier bar
        cancel
    return $ signalBarrier bar ()


-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
--   When it completes all threads have either finished, or have had 'killThread'
--   called on them (but may not have actually died yet).
runPool :: Bool -> Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool deterministic n act = do
    s <- newVar . Just =<< emptyS n deterministic
    done <- newBarrier

    let cleanup = modifyVar_ s $ \s -> do
            -- if someone kills our thread, make sure we kill our child threads
            case s of
                Just s -> mapM_ killThread $ Set.toList $ threads s
                Nothing -> return ()
            return Nothing

    let ghc10793 = do
            -- if this thread dies because it is blocked on an MVar there's a chance we have
            -- a better error in the done barrier, and GHC raised the exception wrongly, see:
            -- https://ghc.haskell.org/trac/ghc/ticket/10793
            sleep 1 -- give it a little bit of time for the finally to run
                    -- no big deal, since the blocked indefinitely takes a while to fire anyway
            res <- waitBarrierMaybe done
            case res of
                Just (Left e) -> throwIO e
                _ -> throwIO BlockedIndefinitelyOnMVar
    handle (\BlockedIndefinitelyOnMVar -> ghc10793) $ flip onException cleanup $ do
        let pool = Pool s done
        addPool PoolStart pool $ act pool
        res <- waitBarrier done
        case res of
            Left e -> throwIO e
            Right s -> addTiming $ "Pool finished (" ++ show (threadsSum s) ++ " threads, " ++ show (threadsMax s) ++ " max)"
