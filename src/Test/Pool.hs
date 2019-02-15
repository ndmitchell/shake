
module Test.Pool(main) where

import Test.Type
import General.Pool

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad
import Data.Either.Extra


main = testSimple $ do
    -- See #474, we should never be running pool actions masked
    let add pool act = addPool PoolStart pool $ do
            Unmasked <- getMaskingState
            act

    forM_ [False,True] $ \deterministic -> do
        -- check that it aims for exactly the limit
        forM_ [1..6] $ \n -> do
            var <- newVar (0,0) -- (maximum, current)
            runPool deterministic n $ \pool ->
                replicateM_ 5 $
                    add pool $ do
                        modifyVar_ var $ \(mx,now) -> return (max (now+1) mx, now+1)
                        -- requires that all tasks get spawned within 0.1s
                        sleep 0.1
                        modifyVar_ var $ \(mx,now) -> return (mx,now-1)
            res <- readVar var
            res === (min n 5, 0)

        -- check that exceptions are immediate
        good <- newVar True
        started <- newBarrier
        stopped <- newBarrier
        res <- try_ $ runPool deterministic 3 $ \pool -> do
                add pool $ do
                    waitBarrier started
                    throwIO Underflow
                add pool $
                    flip finally (signalBarrier stopped ()) $ do
                        signalBarrier started ()
                        sleep 10
                        modifyVar_ good $ const $ return False
        -- note that the pool finishing means we started killing our threads
        -- not that they have actually died
        mapLeft fromException res === Left (Just Underflow)
        waitBarrier stopped
        assertBoolIO (readVar good) "Must be true"

        -- check someone spawned when at zero todo still gets run
        done <- newBarrier
        runPool deterministic 1 $ \pool ->
            add pool $
                add pool $
                    signalBarrier done ()
        assertWithin 1 $ waitBarrier done

        -- check high priority stuff runs first
        res <- newVar ""
        runPool deterministic 1 $ \pool -> do
            let note c = modifyVar_ res $ return . (c:)
            -- deliberately in a random order
            addPool PoolBatch pool $ note 'b'
            addPool PoolException pool $ note 'e'
            addPool PoolStart pool $ note 's'
            addPool PoolStart pool $ note 's'
            addPool PoolResume pool $ note 'r'
            addPool PoolException pool $ note 'e'
        (=== "bssree") =<< readVar res

        -- check that killing a thread pool stops the tasks, bug 545
        -- and propagates the right exception
        thread <- newBarrier
        died <- newBarrier
        done <- newBarrier
        t <- flip forkFinally (signalBarrier died) $ runPool deterministic 1 $ \pool ->
            add pool $
                flip onException (signalBarrier done ()) $ do
                    flip throwTo Overflow =<< waitBarrier thread
                    sleep 10
        signalBarrier thread t
        assertWithin 1 $ waitBarrier done
        res <- assertWithin 1 $ waitBarrier died
        mapLeft fromException res === Left (Just Overflow)

        -- check that killing a thread pool aborts all threads before it returns
        started <- newBarrier
        var <- newVar False
        try_ $ runPool deterministic 2 $ \pool -> do
            add pool $ try_ $ (do signalBarrier started (); sleep 10) `finally` (do sleep 1; writeVar var True)
            add pool $ do waitBarrier started; throw Overflow
        (=== True) =<< readVar var
