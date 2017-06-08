
module Test.Errors(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Data.List
import Control.Monad
import Control.Concurrent
import General.GetOpt
import Control.Exception.Extra hiding (assert)
import System.Directory as IO
import System.Time.Extra
import qualified System.IO.Extra as IO
import Data.Functor
import Prelude

data Args = Die deriving (Eq,Enum,Bounded,Show)

main = shakeTest test optionsEnum $ \args -> do
    "norule" %> \_ ->
        need ["norule_isavailable"]

    "failcreate" %> \_ ->
        return ()

    ["failcreates", "failcreates2"] &%> \_ ->
        writeFile' "failcreates" ""

    "recursive_" %> \out -> need ["intermediate_"]
    "intermediate_" %> \out -> need ["recursive_"]

    "rec1" %> \out -> need ["rec2"]
    "rec2" %> \out -> need ["rec1"]

    "systemcmd" %> \_ ->
        cmd "random_missing_command"

    "stack1" %> \_ -> need ["stack2"]
    "stack2" %> \_ -> need ["stack3"]
    "stack3" %> \_ -> error "crash"

    "staunch1" %> \out -> do
        liftIO $ sleep 0.1
        writeFile' out "test"
    "staunch2" %> \_ -> error "crash"

    let catcher out op = out %> \out -> do
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
    "resource" %> \out ->
        withResource res 1 $
            need ["resource-dep"]

    "overlap.txt" %> \out -> writeFile' out "overlap.txt"
    "overlap.t*" %> \out -> writeFile' out "overlap.t*"
    "overlap.*" %> \out -> writeFile' out "overlap.*"

    "chain.2" %> \out -> do
        src <- readFile' "chain.1"
        if src == "err" then error "err_chain" else writeFileChanged out src
    "chain.3" %> \out -> copyFile' "chain.2" out

    "tempfile" %> \out -> do
        file <- withTempFile $ \file -> do
            liftIO $ assertExists file
            return file
        liftIO $ assertMissing file
        withTempFile $ \file -> do
            liftIO $ assertExists file
            writeFile' out file
            fail "tempfile-died"

    "tempdir" %> \out -> do
        file <- withTempDir $ \dir -> do
            let file = dir </> "foo.txt"
            liftIO $ writeFile (dir </> "foo.txt") ""
                -- will throw if the directory does not exist
            writeFile' out ""
            return file
        liftIO $ assertMissing file

    phony "fail1" $ fail "die1"
    phony "fail2" $ fail "die2"

    when (Die `elem` args) $ action $ error "death error"

    "fresh_dir" %> \out -> liftIO $ createDirectoryIfMissing True out
    "need_dir" %> \out -> do
        liftIO $ createDirectoryIfMissing True "existing_dir"
        need ["existing_dir"]
        writeFile' out ""

    "persist_failure.1" %> \out -> do
        liftIO $ appendFile "persist_failure.log" "[pre]"
        need ["persist_failure.2"]
        liftIO $ appendFile "persist_failure.log" "[post]"
        writeFile' out ""
    "persist_failure.2" %> \out -> do
        src <- readFile' "persist_failure.3"
        liftIO $ print ("persist_failure.3", src)
        if src == "die" then do
            liftIO $ appendFile "persist_failure.log" "[err]"
            fail "die"
        else
            writeFileChanged out src

    "fast_failure" %> \out -> do
        liftIO $ sleep 0.1
        fail "die"
    "slow_success" %> \out -> do
        liftIO $ sleep 20
        writeFile' out ""

    -- not tested by default since only causes an error when idle GC is turned on
    phony "block" $
        liftIO $ putStrLn $ let x = x in x

test build = do
    let crash args parts = assertException parts (build $ "--quiet" : args)
    build ["clean"]
    build ["--sleep"]

    writeFile "chain.1" "x"
    build ["chain.3","--sleep"]
    writeFile "chain.1" "err"
    crash ["chain.3"] ["err_chain"]

    crash ["norule"] ["norule_isavailable"]
    crash ["failcreate"] ["failcreate"]
    crash ["failcreates"] ["failcreates"]
    crash ["recursive_"] ["recursive_","intermediate_","recursive"]
    crash ["rec1","rec2"] ["rec1","rec2","recursive"]
    crash ["systemcmd"] ["systemcmd","random_missing_command"]
    crash ["stack1"] ["stack1","stack2","stack3","crash"]

    b <- IO.doesFileExist "staunch1"
    when b $ removeFile "staunch1"
    crash ["staunch1","staunch2","-j2"] ["crash"]
    assertBoolIO (not <$> IO.doesFileExist "staunch1") "File should not exist, should have crashed first"
    crash ["staunch1","staunch2","-j2","--keep-going","--silent"] ["crash"]
    assertBoolIO (IO.doesFileExist "staunch1") "File should exist, staunch should have let it be created"

    crash ["finally1"] ["die"]
    assertContents "finally1" "1"
    build ["finally2"]
    assertContents "finally2" "1"
    crash ["exception1"] ["die"]
    assertContents "exception1" "1"
    build ["exception2"]
    assertContents "exception2" "0"

    forM_ ["finally3","finally4"] $ \name -> do
        t <- forkIO $ ignore $ build [name,"--exception"]
        retry 10 $ sleep 0.1 >> assertContents name "0"
        throwTo t (IndexOutOfBounds "test")
        retry 10 $ sleep 0.1 >> assertContents name "1"

    crash ["resource"] ["cannot currently call apply","withResource","resource_name"]

    build ["overlap.foo"]
    assertContents "overlap.foo" "overlap.*"
    build ["overlap.txt"]
    assertContents "overlap.txt" "overlap.txt"
    crash ["overlap.txx"] ["key matches multiple rules","overlap.txx"]

    crash ["tempfile"] ["tempfile-died"]
    src <- readFile "tempfile"
    assertMissing src
    build ["tempdir"]

    crash ["--die"] ["Shake","action","death error"]

    putStrLn "## BUILD errors"
    (out,_) <- IO.captureOutput $ build []
    assertBool ("nothing to do" `isInfixOf` out) $ "Expected 'nothing to do', but got: " ++ out

    putStrLn "## BUILD errors fail1 fail2 -k -j2"
    (out,_) <- IO.captureOutput $ try_ $ build ["fail1","fail2","-k","-j2",""]
    assertBool ("die1" `isInfixOf` out && "die2" `isInfixOf` out) $ "Expected 'die1' and 'die2', but got: " ++ out

    crash ["fresh_dir"] ["expected a file, got a directory","fresh_dir"]
    crash ["need_dir"] ["expected a file, got a directory","existing_dir"]

    -- check errors don't persist to the database, #428
    writeFile "persist_failure.log" ""
    writeFile "persist_failure.3" "test"
    build ["persist_failure.1","--sleep"]
    writeFile "persist_failure.3" "die"
    crash ["persist_failure.1","--sleep"] []
    assertContents "persist_failure.log" "[pre][post][err][pre]"
    writeFile "persist_failure.3" "test"
    build ["persist_failure.1","--sleep"]
    assertContents "persist_failure.log" "[pre][post][err][pre]"
    writeFile "persist_failure.3" "more"
    build ["persist_failure.1"]
    assertContents "persist_failure.log" "[pre][post][err][pre][pre][post]"

    -- check a fast failure aborts a slow success
    (t, _) <- duration $ crash ["fast_failure","slow_success","-j2"] ["die"]
    assertBool (t < 10) $ "Took too long, expected < 10, got " ++ show t
