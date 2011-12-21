
module Examples.Test.Basic1(main) where

import Development.Shake
import Examples.Util

main = shaken test $ \obj -> do
    want [obj "AB.txt"]
    obj "AB.txt" *> \out -> do
        need [obj "A.txt", obj "B.txt"]
        text1 <- readFile' $ obj "A.txt"
        text2 <- readFile' $ obj "B.txt"
        writeFile' out $ text1 ++ text2


test build obj = do
    writeFile (obj "A.txt") "AAA"
    writeFile (obj "B.txt") "BBB"
    build []
    assertContents (obj "AB.txt") "AAABBB"
    sleep 1
    appendFile (obj "A.txt") "aaa"
    build []
    assertContents (obj "AB.txt") "AAAaaaBBB"
