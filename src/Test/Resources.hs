
module Test.Resources(main) where

import Development.Shake
import Development.Shake.Core(rulesIO)
import Test.Type
import Control.Monad
import Data.IORef


main = shaken test $ \args obj -> do
    -- test I have good orderings
    do
        r1 <- newResource "test" 2
        r2 <- newResource "test" 2
        unless (r1 < r2 || r2 < r1) $ error "Resources should have a good ordering"

    -- test you are capped to a maximum value
    do
        let cap = 2
        inside <- rulesIO $ newIORef 0
        resource <- newResource "test" cap
        phony "cap" $ need [obj $ "c_file" ++ show i ++ ".txt" | i <- [1..4]]
        obj "c_*.txt" %> \out ->
            withResource resource 1 $ do
                old <- liftIO $ atomicModifyIORef inside $ \i -> (i+1,i)
                when (old >= cap) $ error "Too many resources in use at one time"
                liftIO $ sleep 0.1
                liftIO $ atomicModifyIORef inside $ \i -> (i-1,i)
                writeFile' out ""

    -- test things can still run while you are blocked on a resource
    do
        done <- rulesIO $ newIORef 0
        lock <- newResource "lock" 1
        phony "schedule" $ do
            need $ map (\x -> obj $ "s_" ++ x) $ "lock1":"done":["free" ++ show i | i <- [1..10]] ++ ["lock2"]
        obj "s_done" %> \out -> do
            need [obj "s_lock1",obj "s_lock2"]
            done <- liftIO $ readIORef done
            when (done < 10) $ error "Not all managed to schedule while waiting"
            writeFile' out ""
        obj "s_lock*" %> \out -> do
            withResource lock 1 $ liftIO $ sleep 0.5
            writeFile' out ""
        obj "s_free*" %> \out -> do
            liftIO $ atomicModifyIORef done $ \i -> (i+1,())
            writeFile' out ""


test build obj = do
    build ["-j2","cap","--clean"]
    build ["-j4","cap","--clean"]
    build ["-j10","cap","--clean"]
    build ["-j2","schedule","--clean"]
