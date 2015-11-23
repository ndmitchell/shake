
module Test.Parallel(main) where

import Development.Shake
import Test.Type
import Data.Tuple.Extra
import Control.Monad
import Control.Concurrent.Extra
import Data.IORef


main = shaken test $ \args obj -> do
    want args
    phony "cancel" $ do
        done <- liftIO $ newIORef 4
        lock <- liftIO newLock
        void $ parallel $ replicate 4 $ liftIO $ do
            x <- atomicModifyIORef done $ dupe . pred
            when (x == 2) $ do sleep 0.1; fail "boom"
            withLock lock $ appendFile (obj "cancel") "x"

{-
make sure you cancel parallel actions if any fail
make sure they run in parallel, but only at -j2
make sure they report their execution timing correctly
-}


test build obj = do
    writeFile (obj "cancel") ""
    assertException ["boom"] $ build ["cancel","-j1","--quiet"]
    assertContents (obj "cancel") "x"
