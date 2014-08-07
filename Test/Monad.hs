
module Test.Monad(main) where

import Test.Type
import Development.Shake.Monad

import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.IO.Class


main = shaken test $ \args obj -> return ()


test build obj = do
    let conv x = either (Left . fromException) Right x :: Either (Maybe ArithException) Int
    let dump ro rw = do liftIO . (=== ro) =<< getRO; liftIO . (=== rw) =<< getRW

    -- test the basics plus exception handling
    join $ runRAW 1 "test" $ do
        dump 1 "test"
        putRW "more"
        dump 1 "more"
        res <- tryRAW $ withRO (+3) $ do
            dump 4 "more"
            withRW (++ "x") $ do
                dump 4 "morex"
            dump 4 "more"
            return 100
        liftIO $ conv res === Right 100
        dump 1 "more"
        putRW "new"
        dump 1 "new"
        res <- tryRAW $ withRO (+2) $ do
            dump 3 "new"
            withRW (++ "x") $ do
                dump 3 "newx"
                throwRAW Overflow
            error "Should not have reached here"
            return 9
        liftIO $ conv res === Left (Just Overflow)
        dump 1 "new"
        catchRAW (catchRAW (throwRAW Overflow) $ \_ -> modifyRW (++ "x")) $
            \_ -> modifyRW (++ "y")
        dump 1 "newx"
        catchRAW (catchRAW (throwRAW Overflow) $ \e -> modifyRW (++ "x") >> throwRAW e) $
            \_ -> modifyRW (++ "y")
        dump 1 "newxxy"

    -- test capture
    join $ runRAW 1 "test" $ do
        i <- captureRAW $ \k -> k $ Right 1
        liftIO $ i === 1
        i <- tryRAW $ captureRAW $ \k -> k $ Left $ toException Overflow
        liftIO $ conv i === Left (Just Overflow)
        captureRAW $ \k -> k $ Right ()
        i <- tryRAW $ throwRAW Underflow
        liftIO $ conv i === Left (Just Underflow)
