
module Test.Parallel(main) where

import Development.Shake
import Test.Type
import Data.Tuple.Extra
import Control.Monad
import Control.Concurrent.Extra
import Data.IORef


main = shakeTest_ test $ do
    "AB.txt" %> \out -> do
        -- need [obj "A.txt", obj "B.txt"]
        (text1,text2) <- readFile' "A.txt" `par` readFile' "B.txt"
        writeFile' out $ text1 ++ text2

    phony "cancel" $ do
        writeFile' "cancel" ""
        done <- liftIO $ newIORef 0
        lock <- liftIO newLock
        void $ parallel $ replicate 5 $ liftIO $ do
            x <- atomicModifyIORef done $ dupe . succ
            when (x == 3) $ do sleep 0.1; fail "boom"
            withLock lock $ appendFile "cancel" "x"

    phony "parallel" $ do
        active <- liftIO $ newIORef 0
        peak <- liftIO $ newIORef 0    
        void $ parallel $ replicate 8 $ liftIO $ do
            now <- atomicModifyIORef active $ dupe . succ
            atomicModifyIORef peak $ dupe . max now
            sleep 0.1
            atomicModifyIORef active $ dupe . pred
        peak <- liftIO $ readIORef peak
        writeFile' "parallel" $ show peak


test build = do
    writeFile "A.txt" "AAA"
    writeFile "B.txt" "BBB"
    build ["AB.txt","--sleep"]
    assertContents "AB.txt" "AAABBB"
    appendFile "A.txt" "aaa"
    build ["AB.txt"]
    assertContents "AB.txt" "AAAaaaBBB"

    assertException ["boom"] $ build ["cancel","-j1","--quiet"]
    assertContents "cancel" "xx"
    build ["parallel","-j1"]
    assertContents "parallel" "1"
    build ["parallel","-j5"]
    assertContents "parallel" "5"
