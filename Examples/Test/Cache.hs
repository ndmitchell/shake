
module Examples.Test.Cache(main) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.FileTime
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Examples.Util


main = shaken test $ \args obj -> do
    want $ map obj args
    vowels <- newCache $ \file -> do
        liftIO $ putStrLn $ "@ PRINT: " ++ file
        src <- readFile file
        liftIO $ putStrLn $ "@ APPENDING TO TRACE"
        appendFile (obj "trace.txt") "1"
        return $ length $ filter isDigit src
    obj "*.out*" *> \x -> do
        liftIO $ putStrLn $ "@ REBUILDING " ++ x
        writeFile' x . show =<< vowels (dropExtension x <.> "txt")


test build obj = do
    writeFile (obj "trace.txt") ""
    writeFile (obj "vowels.txt") "abc123a"
    print =<< readFile (obj "vowels.txt")
    print =<< getModTimeMaybe (BS.pack $ obj "vowels.txt")
    build ["vowels.out1","vowels.out2","-j3"]
    assertContents (obj "trace.txt") "1"
    assertContents (obj "vowels.out1") "3"
    assertContents (obj "vowels.out2") "3"

    build ["vowels.out2","-j3"]
    assertContents (obj "trace.txt") "1"
    assertContents (obj "vowels.out1") "3"

    writeFile (obj "vowels.txt") "12xyz34"
    putStrLn "Written, now sleeping!"
    sleep 10
    print =<< readFile (obj "vowels.txt")
    print =<< getModTimeMaybe (BS.pack $ obj "vowels.txt")
    build ["vowels.out2","-j3","--sleep"]
    assertContents (obj "trace.txt") "11"
    assertContents (obj "vowels.out2") "4"

    build ["vowels.out1","-j3","--sleep"]
    assertContents (obj "trace.txt") "111"
    assertContents (obj "vowels.out1") "4"
