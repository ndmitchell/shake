{-# LANGUAGE TupleSections #-}

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
import General.Thread
import qualified Data.Heap as Heap
import qualified Data.HashSet as Set
import Data.IORef.Extra
import System.Random
import Debug.Trace
import GHC.Conc


---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
If any worker throws an exception, must signal to all the other workers
-}

data S = S
    {alive :: !Bool -- True until there's an exception, after which don't spawn more tasks
    ,threads :: !(Set.HashSet Thread) -- IMPORTANT: Must be strict or we leak thread stacks
    ,threadsCount :: {-# UNPACK #-} !Int -- Set.size threads, but in O(1)
    ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads (accounting only)
    ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through (accounting only)
    }


emptyS :: Int -> Bool -> IO S
emptyS n deterministic =
    pure $ S True Set.empty 0 0 0


data Pool = Pool
    !(Var S) -- Current state, 'alive' = False to say we are aborting
    !(Barrier (Either SomeException S)) -- Barrier to signal that we are finished


withPool :: Pool -> (S -> IO (S, IO ())) -> IO ()
withPool (Pool var _) f = join $ modifyVar var $ \s ->
    if alive s then f s else pure (s, pure ())

withPool_ :: Pool -> (S -> IO S) -> IO ()
withPool_ pool act = withPool pool $ fmap (, pure()) . act

threshold :: Float
threshold = 0.05

-- | Add a new task to the pool. See the top of the module for the relative ordering
--   and semantics.
addPool :: PoolPriority -> Pool -> IO a -> IO ()
addPool priority pool@(Pool _ done) act = 
    withPool_ pool $ \s -> do
      traceEventIO $ "Scheduling event with priority: " ++ show priority
      t <- newThreadFinally l mcap act $ \t res -> do
        traceEventIO $ show l ++ " done."
        case res of
          Left e -> withPool_ pool $ \s -> do
            signalBarrier done $ Left e
            pure (remThread t s){alive = False}
          Right _ -> withPool_ pool $ \s -> do
            let s' = remThread t s
            when (threadsCount s' == 0) $
              signalBarrier done $ Right s'{alive = False}
            pure $ s'{alive = threadsCount s' /= 0}
      pure (addThread t s)
  where
      addThread t s = s{threads = Set.insert t $ threads s, threadsCount = threadsCount s + 1
                       ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` (threadsCount s + 1)}
      remThread t s = s{threads = Set.delete t $ threads s, threadsCount = threadsCount s - 1}
      mcap = case priority of
        PoolEstimate t _ | t <= threshold -> Just 0
        _ -> Nothing
      l = case priority of
        PoolEstimate _ s -> s
        _ -> "Unknown"

data PoolPriority
    = PoolException
    | PoolResume
    | PoolStart
    | PoolBatch
    | PoolDeprioritize Double
    | PoolEstimate { estimatedTime :: Float, label :: String }
      deriving (Eq,Ord,Show)

-- | Temporarily increase the pool by 1 thread. Call the cleanup action to restore the value.
--   After calling cleanup you should requeue onto a new thread.
increasePool :: Pool -> IO (IO ())
increasePool pool = pure (pure ())

-- | Make sure the pool cannot run out of tasks (and thus everything finishes) until after the cancel is called.
--   Ensures that a pool that will requeue in time doesn't go idle.
keepAlivePool :: Pool -> IO (IO ())
keepAlivePool pool = do
    bar <- newBarrier
    addPool PoolResume pool $ do
        cancel <- increasePool pool
        waitBarrier bar
        cancel
    pure $ signalBarrier bar ()

-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Bool -> Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool deterministic n act = do
    s <- newVar =<< emptyS n deterministic
    done <- newBarrier
    let pool = Pool s done

    -- if someone kills our thread, make sure we kill our child threads
    let cleanup = join $ modifyVar s $ \s -> pure (s{alive=False}, stopThreads $ Set.toList $ threads s)

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
    flip finally cleanup $ handle (\BlockedIndefinitelyOnMVar -> ghc10793) $ do
        addPool PoolStart pool $ act pool
        res <- waitBarrier done
        case res of
            Left e -> throwIO e
            Right s -> addTiming $ "Pool finished (" ++ show (threadsSum s) ++ " threads, " ++ show (threadsMax s) ++ " max)"
