
module Test.Pool(main) where

import Test.Type
import Development.Shake.Pool

import Control.Concurrent
import Control.Exception hiding (assert)
import Control.Monad


main = shakenCwd test $ \args obj -> return ()


test build obj = do
    let wait = sleep 0.01
    forM_ [False,True] $ \deterministic -> do

        -- check that it aims for exactly the limit
        forM_ [1..6] $ \n -> do
            var <- newMVar (0,0) -- (maximum, current)
            runPool deterministic n $ \pool ->
                forM_ [1..5] $ \i ->
                    addPool pool $ do
                        modifyMVar_ var $ \(mx,now) -> return (max (now+1) mx, now+1)
                        wait
                        modifyMVar_ var $ \(mx,now) -> return (mx,now-1)
            res <- takeMVar var
            res === (min n 5, 0)

        -- check that exceptions are immediate
        self <- myThreadId
        handle (\(ErrorCall msg) -> msg === "pass") $
            runPool deterministic 3 $ \pool -> do
                addPool pool $ do
                    wait
                    error "pass"
                addPool pool $ do
                    wait >> wait
                    throwTo self $ ErrorCall "fail" 
        wait >> wait -- give chance for a delayed exception

        -- check someone spawned when at zero todo still gets run
        done <- newMVar False
        runPool deterministic 1 $ \pool ->
            addPool pool $ do
                wait
                addPool pool $ do
                    wait
                    modifyMVar_ done $ const $ return True
        done <- readMVar done
        assert done "Waiting on someone"

        -- check that killing a thread pool stops the tasks, bug 545
        thread <- newEmptyMVar
        done <- newEmptyMVar
        res <- newMVar True
        t <- forkIO $ flip finally (putMVar done ()) $ runPool deterministic 1 $ \pool ->
            addPool pool $ do
                t <- takeMVar thread
                killThread t
                wait -- allow the thread to die first
                modifyMVar_ res (const $ return False)
        putMVar thread t
        takeMVar done
        wait >> wait >> wait -- allow the bad thread to continue
        res <- readMVar res
        assert res "Early termination"
