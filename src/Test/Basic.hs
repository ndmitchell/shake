
module Test.Basic(main) where

import Development.Shake
import System.FilePath
import Test.Type
import System.Directory as IO
import Data.List
import Data.Maybe
import Control.Monad
import General.Extra


main = shaken test $ \args obj -> do
    want $ map (\x -> fromMaybe (obj x) $ stripPrefix "!" x) args

    obj "AB.txt" %> \out -> do
        need [obj "A.txt", obj "B.txt"]
        text1 <- readFile' $ obj "A.txt"
        text2 <- readFile' $ obj "B.txt"
        writeFile' out $ text1 ++ text2

    obj "twice.txt" %> \out -> do
        let src = obj "once.txt"
        need [src, src]
        copyFile' src out

    obj "once.txt" %> \out -> do
        src <- readFile' $ obj "zero.txt"
        writeFile' out src

    phonys $ \x -> if x /= "halfclean" then Nothing else Just $
        removeFilesAfter (obj "") ["//*e.txt"]

    phony "cleaner" $
        removeFilesAfter (obj "") ["//*"]

    phony (obj "configure") $
        liftIO $ appendFile (obj "configure") "1"

    phony "install" $ do
        need [obj "configure",obj "once.txt"]
        liftIO $ appendFile (obj "install") "1"

    phony "duplicate1" $ need ["duplicate2","duplicate3"]
    phony "duplicate2" $ need ["duplicate3"]
    phony "duplicate3" $ liftIO $ appendFile (obj "duplicate") "1"

    phony "dummy" $
        liftIO $ appendFile (obj "dummy") "1"

    phony "threads" $ do
        x <- getShakeOptions
        writeFile' (obj "threads.txt") $ show $ shakeThreads x

    obj "dummer.txt" %> \out -> do
        need ["dummy","dummy"]
        need ["dummy"]
        liftIO $ appendFile out "1"

    r <- newResource ".log file" 1
    let trace x = withResource r 1 $ liftIO $ appendFile (obj ".log") x
    obj "*.par" %> \out -> do
        trace "["
        (if "unsafe" `isInfixOf` out then unsafeExtraThread else id) $ liftIO $ sleep 0.1
        trace "]"
        writeFile' out out

    obj "sep" </> "1.txt" %> \out -> writeFile' out ""
    obj "sep/2.txt" %> \out -> writeFile' out ""
    [obj "sep" </> "3.txt", obj "sep" </> "4.txt", obj "sep" </> "5.*", obj "sep/6.txt"] |%> \out -> writeFile' out ""
    [obj "sep" </> "7.txt"] |%> \out -> writeFile' out ""

test build obj = do
    writeFile (obj "A.txt") "AAA"
    writeFile (obj "B.txt") "BBB"
    build ["AB.txt","--sleep"]
    assertContents (obj "AB.txt") "AAABBB"
    appendFile (obj "A.txt") "aaa"
    build ["AB.txt"]
    assertContents (obj "AB.txt") "AAAaaaBBB"
    removeFile $ obj "AB.txt"
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

    build ["!halfclean"]
    b <- IO.doesDirectoryExist (obj "")
    assert b "Directory should exist, cleaner should not have removed it"

    build ["!cleaner"]
    sleep 1 -- sometimes takes a while for the file system to notice
    b <- IO.doesDirectoryExist (obj "")
    assert (not b) "Directory should not exist, cleaner should have removed it"

    IO.createDirectory $ obj ""
    writeFile (obj "zero.txt") ""
    build ["configure"]
    build ["!install"]
    build ["!install"]
    assertContents (obj "configure") "111"
    assertContents (obj "install") "11"

    writeFile (obj "dummy.txt") ""
    build ["!dummy"]
    assertContents (obj "dummy") "1"
    build ["!dummy"]
    assertContents (obj "dummy") "11"
    build ["!dummy","!dummy"]
    assertContents (obj "dummy") "111"

    writeFile (obj "dummer.txt") ""
    build ["dummer.txt"]
    assertContents (obj "dummer.txt") "1"
    build ["dummer.txt"]
    assertContents (obj "dummer.txt") "11"

    build ["1.par","2.par","-j1"]
    assertContents (obj ".log") "[][]"
    writeFile (obj ".log") ""
    build ["3.par","4.par","-j2"]
    assertContents (obj ".log") "[[]]"
    writeFile (obj ".log") ""
    processors <- getProcessorCount
    putStrLn $ "getProcessorCount returned " ++ show processors
    when (processors > 1) $ do
        build ["5.par","6.par","-j0"]
        assertContents (obj ".log") "[[]]"

    writeFile (obj ".log") ""
    build ["unsafe1.par","unsafe2.par","-j2"]
    assertContents (obj ".log") "[[]]"

    build ["!threads","-j3"]
    assertContents (obj "threads.txt") "3"
    build ["!threads","-j0"]
    assertContents (obj "threads.txt") (show processors)

    writeFile (obj "duplicate") ""
    build ["!duplicate1","!duplicate3"]
    assertContents (obj "duplicate") "1"

    build $ concat [["sep/" ++ show i ++ ".txt", "sep" </> show i ++ ".txt"] | i <- [1..7]]

    build [] -- should say "no want/action statements, nothing to do" (checked manually)
