
module Test.OrderOnly(main) where

import Development.Shake
import Test.Type
import System.Directory(removeFile)
import Control.Exception.Extra


main = shaken test $ \args obj -> do
    want $ map obj args

    obj "bar.txt" %> \out -> do
        alwaysRerun
        writeFile' out =<< liftIO (readFile $ obj "bar.in")

    obj "foo.txt" %> \out -> do
        let src = obj "bar.txt"
        orderOnly [src]
        writeFile' out =<< liftIO (readFile src)
        need [src]

    obj "baz.txt" %> \out -> do
        let src = obj "bar.txt"
        orderOnly [src]
        liftIO $ appendFile out "x"

    obj "primary.txt" %> \out -> do
        need [obj "source.txt"]
        orderOnly [obj "intermediate.txt"]
        liftIO $ putStrLn "WRITING PRIMARY"
        writeFile' out =<< liftIO (readFile $ obj "intermediate.txt")

    obj "intermediate.txt" %> \out -> do
        liftIO $ putStrLn "WRITING INTERMEDIATE"
        copyFile' (obj "source.txt") out


test build obj = do
    writeFile (obj "bar.in") "in"
    build ["foo.txt","--sleep"]
    assertContents (obj "foo.txt") "in"
    writeFile (obj "bar.in") "out"
    build ["foo.txt","--sleep"]
    assertContents (obj "foo.txt") "out"

    writeFile (obj "baz.txt") ""
    writeFile (obj "bar.in") "in"
    build ["baz.txt","--sleep"]
    assertContents (obj "baz.txt") "x"
    writeFile (obj "bar.in") "out"
    build ["baz.txt"]
    assertContents (obj "baz.txt") "x"

    ignore $ removeFile $ obj "intermediate.txt"
    writeFile (obj "source.txt") "x"
    build ["primary.txt"]
    assertContents (obj "intermediate.txt") "x"
    removeFile $ obj "intermediate.txt"
    build ["primary.txt"]
    assertMissing $ obj "intermediate.txt"
    writeFile (obj "source.txt") "y"
    sleep 5
    build ["primary.txt"]
    sleep 5
    assertContents (obj "intermediate.txt") "y"
