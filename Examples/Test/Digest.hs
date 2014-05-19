
module Examples.Test.Digest(main) where

import Control.Monad
import Development.Shake
import Examples.Util


main = shaken test $ \args obj -> do
    want [obj "Out.txt",obj "Out2.txt"]

    obj "Out.txt" *> \out -> do
        txt <- readFile' $ obj "In.txt"
        liftIO $ appendFile out txt

    [obj "Out1.txt",obj "Out2.txt"] *>> \[out1,out2] -> do
        txt <- readFile' $ obj "In.txt"
        liftIO $ appendFile out1 txt
        liftIO $ appendFile out2 txt


test build obj = do
    let outs = take 1 $ map obj ["Out.txt","Out1.txt","Out2.txt"]
    let writeOut x = forM_ outs $ \out -> writeFile out x
    let writeIn x = writeFile (obj "In.txt") x
    let assertOut x = forM_ outs $ \out -> assertContents out x

    writeOut ""
    writeIn "X"
    build ["--sleep","--digest-and"]
    assertOut "X"

    -- should not involve a hash calculation (sadly no way to test that)
    build ["--sleep","--digest-and"]
    assertOut "X"

    writeIn "X"
    build ["--sleep","--digest-and"]
    assertOut "X"

    writeIn "X"
    build ["--sleep","--digest-or"]
    assertOut "XX"

    writeIn "X"
    build ["--sleep","--digest-and"]
    assertOut "XX"

    writeOut "XX"
    build ["--sleep","--digest-and"]
    assertOut "XX"

    writeOut "Y"
    build ["--sleep","--digest-and"]
    assertOut "YX"

    writeIn "X"
    build ["--sleep","--digest","--no-lint"]
    assertOut "YX"

    writeIn "Z"
    build ["--sleep","--digest-and-input","--no-lint"]
    assertOut "YXZ"

    writeOut "YXZ"
    build ["--sleep","--digest-and-input","--no-lint"]
    assertOut "YXZZ"

    writeIn "Q"
    build ["--sleep","--digest-and-input","--no-lint"]
    assertOut "YXZZQ"

    writeIn "Q"
    build ["--sleep","--digest-and-input","--no-lint"]
    assertOut "YXZZQ"
