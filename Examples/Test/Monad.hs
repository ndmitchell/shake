
module Examples.Test.Monad(main) where

import Examples.Util
import Development.Shake.Monad

import Control.Exception hiding (assert)
import Control.Monad.IO.Class


main = shaken test $ \args obj -> return ()


test build obj = do
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
        liftIO $ (res :: Either ArithException Int) === Right 100
        dump 1 "more"
        putRW "new"
        dump 1 "new"
        res <- tryRAW $ withRO (+2) $ do
            dump 3 "new"
            withRW (++ "x") $ do
                dump 3 "newx"
                throwRAW Overflow
            error "Should not have reached here"
            return "x"
        liftIO $ res === Left Overflow
        dump 1 "new"
