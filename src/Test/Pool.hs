
module Test.Pool(main) where

import Test.Type
import Development.Shake.Internal.Core.Pool

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad


main = shakeTest_ test $ return ()


test build = do
    -- See #474, we should never be running pool actions masked
    let addPool pool act = addPoolMediumPriority pool $ do
            Unmasked <- getMaskingState
            act

    forM_ [False,True] $ \deterministic -> do
        -- check that it aims for exactly the limit
        forM_ [1..6] $ \n -> do
            var <- newMVar (0,0) -- (maximum, current)
            runPool deterministic n $ \pool ->
                forM_ [1..5] $ \i ->
                    addPool pool $ do
                        modifyMVar_ var $ \(mx,now) -> return (max (now+1) mx, now+1)
                        -- requires that all tasks get spawned within 0.1s
                        sleep 0.1
                        modifyMVar_ var $ \(mx,now) -> return (mx,now-1)
            res <- takeMVar var
            res === (min n 5, 0)

        -- check that exceptions are immediate
        good <- newVar True
        started <- newBarrier
        stopped <- newBarrier
        res <- try_ $ runPool deterministic 3 $ \pool -> do
                addPool pool $ do
                    waitBarrier started
                    error "pass"
                addPool pool $
                    flip finally (signalBarrier stopped ()) $ do
                        signalBarrier started ()
                        sleep 10
                        modifyVar_ good $ const $ return False
        -- note that the pool finishing means we started killing our threads
        -- not that they have actually died
        case res of
            Left e | Just (ErrorCall "pass") <- fromException e -> return ()
            _ -> fail $ "Wrong type of result, got " ++ show res
        waitBarrier stopped
        assertBoolIO (readVar good) "Must be true"

        -- check someone spawned when at zero todo still gets run
        done <- newBarrier
        runPool deterministic 1 $ \pool ->
            addPool pool $
                addPool pool $
                    signalBarrier done ()
        assertWithin 1 $ waitBarrier done

        -- check that killing a thread pool stops the tasks, bug 545
        thread <- newBarrier
        died <- newBarrier
        done <- newBarrier
        t <- forkIO $ flip finally (signalBarrier died ()) $ runPool deterministic 1 $ \pool ->
            addPool pool $
                flip onException (signalBarrier done ()) $ do
                    killThread =<< waitBarrier thread
                    sleep 10
        signalBarrier thread t
        assertWithin 1 $ waitBarrier done
        assertWithin 1 $ waitBarrier died
