
-- | Thread pool implementation.
module Development.Shake.Pool(Pool, addPool, addPoolPriority, increasePool, blockPool, runPool) where

import Control.Concurrent
import Control.Exception hiding (blocked)
import Control.Monad
import General.Base
import General.Timing
import qualified Data.HashSet as Set
import System.IO.Unsafe
import System.Random


---------------------------------------------------------------------
-- UNFAIR/RANDOM QUEUE

-- Monad for non-deterministic (but otherwise pure) computations
type NonDet a = IO a

nonDet :: NonDet [Bool]
nonDet = do bs <- unsafeInterleaveIO nonDet
            b <- randomIO
            return $ b:bs

-- Left = deterministic list, Right = non-deterministic tree
data Queue a = Queue [a] (Either [a] (Maybe (Tree a)))

newQueue :: Bool -> Queue a
newQueue deterministic = Queue [] $ if deterministic then Left [] else Right Nothing

enqueuePriority :: a -> Queue a -> Queue a
enqueuePriority x (Queue p t) = Queue (x:p) t

enqueue :: a -> Queue a -> NonDet (Queue a)
enqueue x (Queue p (Left xs)) = return $ Queue p $ Left $ x:xs
enqueue x (Queue p (Right Nothing)) = return $ Queue p $ Right $ Just $ Leaf x
enqueue x (Queue p (Right (Just t))) = do bs <- nonDet; return $ Queue p $ Right $ Just $ insertTree bs x t

dequeue :: Queue a -> Maybe (NonDet (a, Queue a))
dequeue (Queue (p:ps) t) = Just $ return (p, Queue ps t)
dequeue (Queue [] (Left (x:xs))) = Just $ return (x, Queue [] $ Left xs)
dequeue (Queue [] (Left [])) = Nothing
dequeue (Queue [] (Right (Just t))) = Just $ do bs <- nonDet; (x,t) <- return $ removeTree bs t; return (x, Queue [] $ Right t)
dequeue (Queue [] (Right Nothing)) = Nothing


---------------------------------------------------------------------
-- TREE

-- Note that for a Random tree, since everything is Random, Branch x y =~= Branch y x
data Tree a = Leaf a | Branch (Tree a) (Tree a)

insertTree :: [Bool] -> a -> Tree a -> Tree a
insertTree _ x (Leaf y) = Branch (Leaf x) (Leaf y)
insertTree (b:bs) x (Branch y z) = if b then f y z else f z y
    where f y z = Branch y (insertTree bs x z)

removeTree :: [Bool] -> Tree a -> (a, Maybe (Tree a))
removeTree _ (Leaf x) = (x, Nothing)
removeTree (b:bs) (Branch y z) = if b then f y z else f z y
    where
        f y z = case removeTree bs z of
                    (x, Nothing) -> (x, Just y)
                    (x, Just z) -> (x, Just $ Branch y z)

---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
Must spawn a fresh thread to do blockPool
If any worker throws an exception, must signal to all the other workers
-}

data Pool = Pool
    !(Var (Maybe S)) -- ^ Current state, 'Nothing' to say we are aborting
    !(Barrier (Either SomeException S)) -- ^ Barrier to signal that we are

data S = S
    {threads :: !(Set.HashSet ThreadId) -- IMPORTANT: Must be strict or we leak thread stacks
    ,threadsLimit :: {-# UNPACK #-} !Int -- user supplied thread limit
    ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads
    ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through
    ,working :: {-# UNPACK #-} !Int -- threads which are actively working
    ,blocked :: {-# UNPACK #-} !Int -- threads which are blocked
    ,todo :: !(Queue (IO ()))
    }


emptyS :: Int -> Bool -> S
emptyS n deterministic = S Set.empty n 0 0 0 0 $ newQueue deterministic


-- | Given a pool, and a function that breaks the S invariants, restore them
--   They are only allowed to touch working or todo
step :: Pool -> (S -> NonDet S) -> IO ()
step pool@(Pool var done) op = do
    let onVar act = modifyVar_ var $ maybe (return Nothing) act
    onVar $ \s -> do
        s <- op s
        res <- maybe (return Nothing) (fmap Just) $ dequeue $ todo s
        case res of
            Just (now, todo2) | working s < threadsLimit s -> do
                -- spawn a new worker
                t <- forkIO $ do
                    t <- myThreadId
                    res <- try now
                    case res of
                        Left e -> onVar $ \s -> do
                            mapM_ killThread $ Set.toList $ Set.delete t $ threads s
                            signalBarrier done $ Left e
                            return Nothing
                        Right _ -> step pool $ \s -> return s{working = working s - 1, threads = Set.delete t $ threads s}
                let threads2 = Set.insert t $ threads s
                return $ Just s{working = working s + 1, todo = todo2, threads = threads2
                               ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` Set.size threads2}
            Nothing | working s == 0 && blocked s == 0 -> do
                signalBarrier done $ Right s
                return Nothing
            _ -> return $ Just s


-- | Add a new task to the pool, may be cancelled by sending it an exception
addPool :: Pool -> IO a -> IO ()
addPool pool act = step pool $ \s -> do
    todo <- enqueue (void act) (todo s)
    return s{todo = todo}

-- | Add a new task to the pool, may be cancelled by sending it an exception.
--   Takes priority over everything else.
addPoolPriority :: Pool -> IO a -> IO ()
addPoolPriority pool act = step pool $ \s -> do
    todo <- return $ enqueuePriority (void act) (todo s)
    return s{todo = todo}


-- | Temporarily increase the pool by 1 thread. Call the cleanup action to restore the value.
--   After calling cleanup you should requeue onto a new thread.
increasePool :: Pool -> IO (IO ())
increasePool pool = do
    step pool $ \s -> return s{threadsLimit = threadsLimit s + 1}
    return $ step pool $ \s -> return s{threadsLimit = threadsLimit s - 1}


-- | A blocking action is being run while on the pool, yield your thread.
--   Should only be called by an action under addPool.
--
--   If the first part of the result is True then the result is sufficiently high
--   priority that you may exceed the pool limit to get it done immediately.
--   Always the result of a child thread raising an error, which will probably
--   raise an error in the parent.
blockPool :: Pool -> IO (Bool, a) -> IO a
blockPool pool act = do
    step pool $ \s -> return s{working = working s - 1, blocked = blocked s + 1}
    (urgent,res) <- act
    var <- newBarrier
    let act = do
            step pool $ \s -> return s{working = working s + 1, blocked = blocked s - 1}
            signalBarrier var ()
    if urgent then
        act -- may exceed the pool count
     else
        step pool $ \s -> return s{todo = enqueuePriority act $ todo s}
    waitBarrier var
    return res


-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Bool -> Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool deterministic n act = do
    s <- newVar $ Just $ emptyS n deterministic
    let cleanup = modifyVar_ s $ \s -> do
            -- if someone kills our thread, make sure we kill our child threads
            case s of
                Just s -> mapM_ killThread $ Set.toList $ threads s
                Nothing -> return ()
            return Nothing
    flip onException cleanup $ do
        res <- newBarrier
        let pool = Pool s res
        addPool pool $ act pool
        res <- waitBarrier res
        case res of
            Left e -> throw e
            Right s -> addTiming $ "Pool finished (" ++ show (threadsSum s) ++ " threads, " ++ show (threadsMax s) ++ " max)"
