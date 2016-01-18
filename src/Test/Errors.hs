
module Test.Errors(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Data.List
import Control.Monad
import Control.Concurrent
import Control.Exception.Extra hiding (assert)
import System.Directory as IO
import qualified System.IO.Extra as IO


main = shakenCwd test $ \args obj -> do
    want args
    obj "norule" %> \_ ->
        need [obj "norule_isavailable"]

    obj "failcreate" %> \_ ->
        return ()

    [obj "failcreates", obj "failcreates2"] &%> \_ ->
        writeFile' (obj "failcreates") ""

    obj "recursive" %> \out -> need [obj "intermediate"]
    obj "intermediate" %> \out -> need [obj "recursive"]

    obj "systemcmd" %> \_ ->
        cmd "random_missing_command"

    obj "stack1" %> \_ -> need [obj "stack2"]
    obj "stack2" %> \_ -> need [obj "stack3"]
    obj "stack3" %> \_ -> error "crash"

    obj "staunch1" %> \out -> do
        liftIO $ sleep 0.1
        writeFile' out "test"
    obj "staunch2" %> \_ -> error "crash"

    let catcher out op = obj out %> \out -> do
            writeFile' out "0"
            op $ do src <- IO.readFile' out; writeFile out $ show (read src + 1 :: Int)
    catcher "finally1" $ actionFinally $ fail "die"
    catcher "finally2" $ actionFinally $ return ()
    catcher "finally3" $ actionFinally $ liftIO $ sleep 10
    catcher "finally4" $ actionFinally $ need ["wait"]
    "wait" ~> do liftIO $ sleep 10
    catcher "exception1" $ actionOnException $ fail "die"
    catcher "exception2" $ actionOnException $ return ()

    res <- newResource "resource_name" 1
    obj "resource" %> \out ->
        withResource res 1 $
            need ["resource-dep"]

    obj "overlap.txt" %> \out -> writeFile' out "overlap.txt"
    obj "overlap.t*" %> \out -> writeFile' out "overlap.t*"
    obj "overlap.*" %> \out -> writeFile' out "overlap.*"
    alternatives $ do
        obj "alternative.t*" %> \out -> writeFile' out "alternative.txt"
        obj "alternative.*" %> \out -> writeFile' out "alternative.*"

    obj "chain.2" %> \out -> do
        src <- readFile' $ obj "chain.1"
        if src == "err" then error "err_chain" else writeFileChanged out src
    obj "chain.3" %> \out -> copyFile' (obj "chain.2") out

    obj "tempfile" %> \out -> do
        file <- withTempFile $ \file -> do
            liftIO $ assertExists file
            return file
        liftIO $ assertMissing file
        withTempFile $ \file -> do
            liftIO $ assertExists file
            writeFile' out file
            fail "tempfile-died"

    obj "tempdir" %> \out -> do
        file <- withTempDir $ \dir -> do
            let file = dir </> "foo.txt"
            liftIO $ writeFile (dir </> "foo.txt") ""
                -- will throw if the directory does not exist
            writeFile' out ""
            return file
        liftIO $ assertMissing file

    phony "fail1" $ fail "die1"
    phony "fail2" $ fail "die2"

    when ("die" `elem` args) $ action $ error "death error"


    -- not tested by default since only causes an error when idle GC is turned on
    phony "block" $
        liftIO $ putStrLn $ let x = x in x

test build obj = do
    let crash args parts = assertException parts (build $ "--quiet" : args)
    build ["clean"]
    build ["--sleep"]

    writeFile (obj "chain.1") "x"
    build ["chain.3","--sleep"]
    writeFile (obj "chain.1") "err"
    crash ["chain.3"] ["err_chain"]

    crash ["norule"] ["norule_isavailable"]
    crash ["failcreate"] ["failcreate"]
    crash ["failcreates"] ["failcreates"]
    crash ["recursive"] ["recursive","intermediate"]
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

    forM_ ["finally3","finally4"] $ \name -> do
        t <- forkIO $ ignore $ build [name,"--exception"]
        retry 10 $ sleep 0.1 >> assertContents (obj name) "0"
        throwTo t (IndexOutOfBounds "test")
        retry 10 $ sleep 0.1 >> assertContents (obj name) "1"

    crash ["resource"] ["cannot currently call apply","withResource","resource_name"]

    build ["overlap.foo"]
    assertContents (obj "overlap.foo") "overlap.*"
    build ["overlap.txt"]
    assertContents (obj "overlap.txt") "overlap.txt"
    crash ["overlap.txx"] ["key matches multiple rules","overlap.txx"]
    build ["alternative.foo","alternative.txt"]
    assertContents (obj "alternative.foo") "alternative.*"
    assertContents (obj "alternative.txt") "alternative.txt"

    crash ["tempfile"] ["tempfile-died"]
    src <- readFile $ obj "tempfile"
    assertMissing src
    build ["tempdir"]

    crash ["die"] ["Shake","action","death error"]

    putStrLn "## BUILD errors"
    (out,_) <- IO.captureOutput $ build []
    assert ("nothing to do" `isInfixOf` out) $ "Expected 'nothing to do', but got: " ++ out

    putStrLn "## BUILD errors fail1 fail2 -k -j2"
    (out,_) <- IO.captureOutput $ try_ $ build ["fail1","fail2","-k","-j2",""]
    assert ("die1" `isInfixOf` out && "die2" `isInfixOf` out) $ "Expected 'die1' and 'die2', but got: " ++ out
