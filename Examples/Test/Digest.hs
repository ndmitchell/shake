
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
    let outs = map obj ["Out.txt","Out1.txt","Out2.txt"]
    let writeOut x = forM_ outs $ \out -> writeFile out x
    let assertOut x = forM_ outs $ \out -> assertContents out x

    writeFile (obj "In.txt") "X"
    writeOut "X"
    build ["--sleep","--digest-or"]
    assertOut "XX"

    writeFile (obj "In.txt") "X"
    build ["--sleep","--digest-and"]
    assertOut "XX"

    writeOut "XX"
    build ["--sleep","--digest-and"]
    assertOut "XX"

    writeOut "Y"
    build ["--sleep","--digest-and"]
    assertOut "YX"

    writeFile (obj "In.txt") "X"
    build ["--sleep","--digest","--no-lint"]
    assertOut "YX"
