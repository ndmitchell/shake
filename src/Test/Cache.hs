
module Test.Cache(main) where

import Development.Shake
import Development.Shake.FilePath
import Data.Char
import Test.Type


main = shakeTest_ test $ do
    vowels <- newCache $ \file -> do
        src <- readFile' file
        liftIO $ appendFile "trace.txt" "1"
        return $ length $ filter isDigit src
    "*.out*" %> \x ->
        writeFile' x . show =<< vowels (dropExtension x <.> "txt")


test build = do
    writeFile "trace.txt" ""
    writeFile "vowels.txt" "abc123a"
    build ["vowels.out1","vowels.out2","-j3","--sleep"]
    assertContents "trace.txt" "1"
    assertContents "vowels.out1" "3"
    assertContents "vowels.out2" "3"

    build ["vowels.out2","-j3"]
    assertContents "trace.txt" "1"
    assertContents "vowels.out1" "3"

    writeFile "vowels.txt" "12xyz34"
    build ["vowels.out2","-j3","--sleep"]
    assertContents "trace.txt" "11"
    assertContents "vowels.out2" "4"

    build ["vowels.out1","-j3","--sleep"]
    assertContents "trace.txt" "111"
    assertContents "vowels.out1" "4"
