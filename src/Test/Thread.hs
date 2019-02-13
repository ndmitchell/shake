{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Test.Thread(main) where

import General.Cleanup
import General.Thread
import Control.Exception.Extra
import Control.Concurrent.Extra
import Data.Either.Extra
import Data.IORef
import Test.Type


main = testSimple $ do
    ref <- newIORef 0
    let finish = atomicModifyIORef ref $ \x -> (x+1, ())
    let finished want = do
            got <- atomicModifyIORef ref (0,)
            want === got

    pauser <- newEmptyMVar
    let pause = takeMVar pauser
    let unpause = putMVar pauser ()

    let isAnswer x act = do
            r <- assertWithin 1 $ try_ act
            mapLeft show r === Right x
    let isException x act = do
            r <- assertWithin 1 $ try_ act
            mapLeft fromException r === Left (Just x)

    putStrLn "## allocateThread, spanwed finishes first"
    isAnswer 1 $ withCleanup $ \cleanup -> do
        allocateThread cleanup finish
        sleep 0.1
        return 1
    finished 1

    putStrLn "## allocateThread, main finishes first"
    isAnswer 1 $ withCleanup $ \cleanup -> do
        allocateThread cleanup $ (unpause >> sleep 100) `finally` finish
        pause
        return 1
    finished 1

    putStrLn "## allocateThread, spawned throws an exception"
    isException Overflow $ withCleanup $ \cleanup -> do
        allocateThread cleanup $ pause >> throw Overflow
        (unpause >> sleep 100) `finally` finish
    finished 1

    putStrLn "## allocateThread, main throws an exception"
    isException Overflow $ withCleanup $ \cleanup -> do
        allocateThread cleanup $ (unpause >> sleep 100) `finally` finish
        pause
        throw Overflow
        return 1
    finished 1
