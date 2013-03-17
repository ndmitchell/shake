
module Examples.Test.Basic(main) where

import Development.Shake
import Examples.Util
import System.Directory as IO
import Data.List
import Data.Maybe


main = shaken test $ \args obj -> do
    want $ map (\x -> fromMaybe (obj x) $ stripPrefix "!" x) args

    obj "AB.txt" *> \out -> do
        need [obj "A.txt", obj "B.txt"]
        text1 <- readFile' $ obj "A.txt"
        text2 <- readFile' $ obj "B.txt"
        writeFile' out $ text1 ++ text2

    obj "twice.txt" *> \out -> do
        let src = obj "once.txt"
        need [src, src]
        copyFile' src out

    obj "once.txt" *> \out -> do
        src <- readFile' $ obj "zero.txt"
        writeFile' out src

    phony "cleaner" $ do
        removeFilesAfter (obj "") ["//*"]

    phony (obj "configure") $ do
        liftIO $ appendFile (obj "configure") "1"

    phony "install" $ do
        need [obj "configure",obj "once.txt"]
        liftIO $ appendFile (obj "install") "1"


test build obj = do
    writeFile (obj "A.txt") "AAA"
    writeFile (obj "B.txt") "BBB"
    build ["AB.txt","--sleep"]
    assertContents (obj "AB.txt") "AAABBB"
    appendFile (obj "A.txt") "aaa"
    build ["AB.txt"]
    assertContents (obj "AB.txt") "AAAaaaBBB"

    writeFile (obj "zero.txt") "xxx"
    build ["twice.txt","--sleep"]
    assertContents (obj "twice.txt") "xxx"
    writeFile (obj "zero.txt") "yyy"
    build ["once.txt","--sleep"]
    assertContents (obj "twice.txt") "xxx"
    assertContents (obj "once.txt") "yyy"
    writeFile (obj "zero.txt") "zzz"
    build ["once.txt","twice.txt","--sleep"]
    assertContents (obj "twice.txt") "zzz"
    assertContents (obj "once.txt") "zzz"

    removeFile $ obj "twice.txt"
    build ["twice.txt"]
    assertContents (obj "twice.txt") "zzz"

    show shakeOptions === show shakeOptions

    build ["!cleaner"]
    b <- IO.doesDirectoryExist (obj "")
    assert (not b) "Directory should exist, cleaner should have removed it"

    IO.createDirectory $ obj ""
    writeFile (obj "zero.txt") ""
    build ["configure"]
    build ["!install"]
    build ["!install"]
    assertContents (obj "configure") "111"
    assertContents (obj "install") "11"
