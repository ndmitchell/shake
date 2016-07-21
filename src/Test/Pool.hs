
module Test.Pool(main) where

import Test.Type
import Development.Shake.Internal.Core.Pool

import Control.Concurrent.Extra
import Control.Exception
import Control.Monad


main = shakenCwd test $ \args obj -> return ()


test build obj = do
    forM_ [False,True] $ \deterministic -> do
        -- check that it aims for exactly the limit
        forM_ [1..6] $ \n -> do
            var <- newMVar (0,0) -- (maximum, current)
            runPool deterministic n $ \pool ->
                forM_ [1..5] $ \i ->
                    addPoolMediumPriority pool $ do
                        modifyMVar_ var $ \(mx,now) -> return (max (now+1) mx, now+1)
                        -- requires that all tasks get spawned within 0.1s
                        sleep 0.1
                        modifyMVar_ var $ \(mx,now) -> return (mx,now-1)
            res <- takeMVar var
            res === (min n 5, 0)

        -- check that exceptions are immediate
        replicateM_ 100 $ do
            done <- newVar False
            started <- newBarrier
            print "here1"
            handle (\(ErrorCall msg) -> do print ("here2",msg); msg === "pass") $
                runPool deterministic 3 $ \pool -> do
                    addPoolMediumPriority pool $ do
                        print "here8"
                        waitBarrier started
                        print "here3"
                        error "pass"
                    addPoolMediumPriority pool $
                        flip onException (do print "here7"; modifyVar_ done $ const $ return True) $ do
                            print "here4"
                            signalBarrier started ()
                            print "here5"
                            sleep 10
                            print "here6"
            assertBoolIO (readVar done) "Must be true"

        -- check someone spawned when at zero todo still gets run
        done <- newBarrier
        runPool deterministic 1 $ \pool ->
            addPoolMediumPriority pool $ do
                addPoolMediumPriority pool $ do
                    signalBarrier done ()
        assertWithin 1 $ waitBarrier done

        -- check that killing a thread pool stops the tasks, bug 545
        thread <- newBarrier
        died <- newBarrier
        done <- newBarrier
        t <- forkIO $ flip finally (signalBarrier died ()) $ runPool deterministic 1 $ \pool ->
            addPoolMediumPriority pool $ do
                flip onException (signalBarrier done ()) $ do
                    killThread =<< waitBarrier thread
                    sleep 10
        signalBarrier thread t
        assertWithin 1 $ waitBarrier done
        assertWithin 1 $ waitBarrier died
