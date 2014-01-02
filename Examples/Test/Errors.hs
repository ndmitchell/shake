{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Test.Errors(main) where

import Development.Shake
import Examples.Util
import Control.Monad
import System.Directory as IO


main = shaken test $ \args obj -> do
    want $ map obj args

    obj "norule" *> \_ ->
        need [obj "norule_isavailable"]

    obj "failcreate" *> \_ ->
        return ()

    [obj "failcreates", obj "failcreates2"] *>> \_ ->
        writeFile' (obj "failcreates") ""

    obj "recursive" *> \out ->
        need [out]

    obj "systemcmd" *> \_ ->
        cmd "random_missing_command"

    obj "stack1" *> \_ -> need [obj "stack2"]
    obj "stack2" *> \_ -> need [obj "stack3"]
    obj "stack3" *> \_ -> error "crash"

    obj "staunch1" *> \out -> do
        liftIO $ sleep 0.1
        writeFile' out "test"
    obj "staunch2" *> \_ -> error "crash"

    let catcher out op die = obj out *> \out -> do
            writeFile' out "0"
            op (when die $ error "die") (writeFile out "1")
    catcher "finally1" actionFinally True
    catcher "finally2" actionFinally False
    catcher "exception1" actionOnException True
    catcher "exception2" actionOnException False

    res <- newResource "resource_name" 1
    obj "resource" *> \out -> do
        withResource res 1 $
            need ["resource-dep"]

    obj "overlap.txt" *> \out -> writeFile' out "overlap.txt"
    obj "overlap.*" *> \out -> writeFile' out "overlap.*"

test build obj = do
    let crash args parts = assertException parts (build $ "--quiet" : args)

    crash ["norule"] ["norule_isavailable"]
    crash ["failcreate"] ["failcreate"]
    crash ["failcreates"] ["failcreates"]
    crash ["recursive"] ["recursive"]
    crash ["systemcmd"] ["systemcmd","random_missing_command"]
    crash ["stack1"] ["stack1","stack2","stack3","crash"]

    b <- IO.doesFileExist $ obj "staunch1"
    when b $ removeFile $ obj "staunch1"
    crash ["staunch1","staunch2","-j2"] ["crash"]
    b <- IO.doesFileExist $ obj "staunch1"
    assert (not b) "File should not exist, should have crashed first"
    crash ["staunch1","staunch2","-j2","--keep-going","--silent"] ["crash"]
    b <- IO.doesFileExist $ obj "staunch1"
    assert b "File should exist, staunch should have let it be created"

    crash ["finally1"] ["die"]
    assertContents (obj "finally1") "1"
    build ["finally2"]
    assertContents (obj "finally2") "1"
    crash ["exception1"] ["die"]
    assertContents (obj "exception1") "1"
    build ["exception2"]
    assertContents (obj "exception2") "0"

    crash ["resource"] ["cannot currently call apply","withResource","resource_name"]

    build ["overlap.foo"]
    assertContents (obj "overlap.foo") "overlap.*"
    crash ["overlap.txt"] ["key matches multiple rules","overlap.txt"]
