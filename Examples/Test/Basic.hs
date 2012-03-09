
module Examples.Test.Basic(main) where

import Development.Shake
import Examples.Util
import System.Directory


main = shaken test $ \args obj -> do
    want $ map obj args
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
    build ["AB.txt"]
    assertContents (obj "AB.txt") "AAABBB"
    sleepFileTime
    appendFile (obj "A.txt") "aaa"
    build ["AB.txt"]
    assertContents (obj "AB.txt") "AAAaaaBBB"

    writeFile (obj "zero.txt") "xxx"
    build ["twice.txt"]
    assertContents (obj "twice.txt") "xxx"
    sleepFileTime
    writeFile (obj "zero.txt") "yyy"
    build ["once.txt"]
    assertContents (obj "twice.txt") "xxx"
    assertContents (obj "once.txt") "yyy"
    sleepFileTime
    writeFile (obj "zero.txt") "zzz"
    build ["once.txt","twice.txt"]
    assertContents (obj "twice.txt") "zzz"
    assertContents (obj "once.txt") "zzz"

    removeFile $ obj "twice.txt"
    build ["twice.txt"]
    assertContents (obj "twice.txt") "zzz"
