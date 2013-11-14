
module Examples.Test.OrderOnly(main) where

import Development.Shake
import Examples.Util


main = shaken test $ \args obj -> do
    want $ map obj args

    obj "bar.txt" *> \out -> do
        alwaysRerun
        writeFile' out =<< liftIO (readFile $ obj "bar.in")

    obj "foo.txt" *> \out -> do
        let src = obj "bar.txt"
        orderOnly [src]
        writeFile' out =<< liftIO (readFile src)
        need [src]

    obj "baz.txt" *> \out -> do
        let src = obj "bar.txt"
        orderOnly [src]
        liftIO $ appendFile out "x"


test build obj = do
    writeFile (obj "bar.in") "in"
    build ["foo.txt"]
    assertContents (obj "foo.txt") "in"
    writeFile (obj "bar.in") "out"
    build ["foo.txt"]
    assertContents (obj "foo.txt") "out"

    writeFile (obj "baz.txt") ""
    writeFile (obj "bar.in") "in"
    build ["baz.txt"]
    assertContents (obj "baz.txt") "x"
    writeFile (obj "bar.in") "out"
    build ["baz.txt"]
    assertContents (obj "baz.txt") "x"
