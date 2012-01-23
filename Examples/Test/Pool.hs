
module Examples.Test.Pool(main) where

import Examples.Util
import Development.Shake.Pool

import Control.Concurrent
import Control.Exception hiding (assert)
import Control.Monad


main = shaken test $ \args obj -> return ()


test build obj = do
    let wait = sleep 0.01

    -- check that it aims for exactly the limit
    forM_ [1..6] $ \n -> do
        var <- newMVar (0,0) -- (maximum, current)
        runPool n $ \pool ->
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
        runPool 3 $ \pool -> do
            addPool pool $ do
                wait
                error "pass"
            addPool pool $ do
                wait >> wait
                throwTo self $ ErrorCall "fail" 
    wait >> wait -- give chance for a delayed exception

    -- check blocking works
    done <- newMVar False
    runPool 1 $ \pool -> do
        var <- newEmptyMVar
        addPool pool $ do
            addPool pool $ do
                wait
                putMVar var ()
            blockPool pool $ takeMVar var
            modifyMVar_ done $ const $ return True
    done <- readMVar done
    assert done "Blocking works"

    -- check someone spawned when at zero todo still gets run
    done <- newMVar False
    runPool 1 $ \pool ->
        addPool pool $ do
            wait
            addPool pool $ do
                wait
                modifyMVar_ done $ const $ return True
    done <- readMVar done
    assert done "Waiting on someone works"
