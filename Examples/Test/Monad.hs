
module Examples.Test.Monad(main) where

import Examples.Util
import Development.Shake.Monad

import Control.Exception hiding (assert)
import Control.Monad.IO.Class


main = shaken test $ \args obj -> return ()


test build obj = do
    let conv x = either (Left . fromException) Right x :: Either (Maybe ArithException) Int
    runRAW 1 "test" $ do
        let dump ro rw = do liftIO . (=== ro) =<< getRO; liftIO . (=== rw) =<< getRW
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
