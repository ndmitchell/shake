
-- | Thread pool implementation.
module Development.Shake.Pool(Pool, addPool, blockPool, runPool) where

import Control.Concurrent
import Control.Exception hiding (blocked)
import Control.Monad
import Development.Shake.Locks
import qualified Data.HashSet as Set


---------------------------------------------------------------------
-- SUPER QUEUE

-- FIXME: The super queue should use randomness for the normal priority pile

data SuperQueue a = SuperQueue [a] [a]

newSuperQueue :: SuperQueue a
newSuperQueue = SuperQueue [] []

enqueuePriority :: a -> SuperQueue a -> SuperQueue a
enqueuePriority x (SuperQueue p n) = SuperQueue (x:p) n

enqueue :: a -> SuperQueue a -> SuperQueue a
enqueue x (SuperQueue p n) = SuperQueue p (x:n)

dequeue :: SuperQueue a -> Maybe (a, SuperQueue a)
dequeue (SuperQueue (p:ps) ns) = Just (p, SuperQueue ps ns)
dequeue (SuperQueue [] (n:ns)) = Just (n, SuperQueue [] ns)
dequeue (SuperQueue [] []) = Nothing


---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
Must spawn a fresh thread to do blockPool
If any worker throws an exception, must signal to all the other workers
-}

data Pool = Pool Int (Var (Maybe S)) (Barrier (Maybe SomeException))

data S = S
    {threads :: Set.HashSet ThreadId
    ,working :: Int -- threads which are actively working
    ,blocked :: Int -- threads which are blocked
    ,todo :: SuperQueue (IO ())
    }


emptyS :: S
emptyS = S Set.empty 0 0 newSuperQueue


-- | Given a pool, and a function that breaks the S invariants, restore them
--   They are only allowed to touch working or todo
step :: Pool -> (S -> S) -> IO ()
step pool@(Pool n var done) op = do
    let onVar act = modifyVar_ var $ maybe (return Nothing) act
    onVar $ \s -> do
        s <- return $ op s
        case dequeue (todo s) of
            Just (now, todo2) | working s < n -> do
                -- spawn a new worker
                t <- forkIO $ do
                    t <- myThreadId
                    res <- try now
                    case res of
                        Left e -> onVar $ \s -> do
                            mapM_ killThread $ Set.toList $ Set.delete t $ threads s
                            signalBarrier done $ Just e
                            return Nothing
                        Right _ -> step pool $ \s -> s{working = working s - 1, threads = Set.delete t $ threads s}
                return $ Just s{working = working s + 1, todo = todo2, threads = Set.insert t $ threads s}
            Nothing | working s == 0 && blocked s == 0 -> do
                signalBarrier done Nothing
                return Nothing
            _ -> return $ Just s


-- | Add a new task to the pool
addPool :: Pool -> IO a -> IO ()
addPool pool act = step pool $ \s -> s{todo = enqueue (act >> return ()) (todo s)}


-- | A blocking action is being run while on the pool, yeild your thread.
--   Should only be called by an action under addPool.
blockPool :: Pool -> IO a -> IO a
blockPool pool act = do
    step pool $ \s -> s{working = working s - 1, blocked = blocked s + 1}
    res <- act
    var <- newBarrier
    let act = do
            step pool $ \s -> s{working = working s + 1, blocked = blocked s - 1}
            signalBarrier var ()
    step pool $ \s -> s{todo = enqueuePriority act $ todo s}
    waitBarrier var
    return res


-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool n act = do
    s <- newVar $ Just emptyS
    res <- newBarrier
    let pool = Pool n s res
    addPool pool $ act pool
    res <- waitBarrier res
    case res of
        Nothing -> return ()
        Just e -> throw e
