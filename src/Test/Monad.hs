
module Test.Monad(main) where

import Test.Type
import Development.Shake.Internal.Core.Monad

import Data.IORef
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class


main = shakeTest_ test $ return ()


run :: ro -> rw -> RAW ro rw a -> IO a
run ro rw m = do
    res <- newEmptyMVar
    runRAW ro rw m $ void . tryPutMVar res
    either throwIO return =<< readMVar res


test build = do
    let conv x = either (Left . fromException) Right x :: Either (Maybe ArithException) Int
    let dump rw = liftIO . (=== rw) =<< getRW

    -- test the basics plus exception handling
    run 1 "test" $ do
        dump "test"
        putRW "more"
        dump "more"
        res <- tryRAW $ do
            dump "more"
            modifyRW (++ "x")
            dump "morex"
            return 100
        liftIO $ conv res === Right 100
        dump "morex"
        putRW "new"
        dump "new"
        res <- tryRAW $ do
            dump "new"
            modifyRW (++ "z")
            dump "newz"
            throwRAW Overflow
            error "Should not have reached here"
            return 9
        liftIO $ conv res === Left (Just Overflow)
        dump "newz"
        catchRAW (catchRAW (throwRAW Overflow) $ \_ -> modifyRW (++ "x")) $
            \_ -> modifyRW (++ "y")
        dump "newzx"
        catchRAW (catchRAW (throwRAW Overflow) $ \e -> modifyRW (++ "x") >> throwRAW e) $
            \_ -> modifyRW (++ "y")
        dump "newzxxy"

    -- test capture
    run 1 "test" $ do
        i <- captureRAW $ \k -> k $ Right 1
        liftIO $ i === 1
        i <- tryRAW $ captureRAW $ \k -> k $ Left $ toException Overflow
        liftIO $ conv i === Left (Just Overflow)
        captureRAW $ \k -> k $ Right ()
        i <- tryRAW $ throwRAW Underflow
        liftIO $ conv i === Left (Just Underflow)

    -- catch does not scope too far
    res <- try $ run 1 "test" $
        fmap (either show id) $ tryRAW $ captureRAW $ \k -> throwIO Overflow
    res === Left Overflow
    res <- try $ run 1 "test" $ do
        captureRAW $ \k -> throwIO Overflow
        return "x"
    res === Left Overflow
    -- test for GHC bug 11555
    runRAW 1 "test" (throw Overflow :: RAW Int String ()) $ \res ->
        either (Left . fromException) Right res === Left (Just Overflow)

    -- catch works properly if continuation called multiple times
    ref <- newIORef []
    run 1 "test" $
        flip catchRAW (const $ liftIO $ modifyIORef ref ('x':)) $ do
            captureRAW $ \k -> do
                k $ Right ()
                k $ Right ()
                k $ Left $ toException Overflow
                k $ Right ()
                k $ Left $ toException Overflow
            flip catchRAW (const $ liftIO $ modifyIORef ref ('y':)) $ throwRAW $ toException Overflow
    (===) "xyxyy" =<< readIORef ref
