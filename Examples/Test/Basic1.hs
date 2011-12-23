
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
    let f b pat file = assert (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
    f True "//*.c" "foo/bar/baz.c"
    f True "*.c" "baz.c"
    f True "//*.c" "baz.c"
    f True "test.c" "test.c"
    f False "*.c" "foor/bar.c"
    f False "*/*.c" "foo/bar/baz.c"

    writeFile (obj "A.txt") "AAA"
    writeFile (obj "B.txt") "BBB"
    build []
    assertContents (obj "AB.txt") "AAABBB"
    sleep 1
    appendFile (obj "A.txt") "aaa"
    build []
    assertContents (obj "AB.txt") "AAAaaaBBB"
