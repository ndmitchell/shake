
module Test.Lint(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Control.Exception hiding (assert)
import System.Directory as IO
import Control.Monad
import Data.Maybe


main = shaken test $ \args obj -> do
    want $ map obj args

    addOracle $ \() -> do
        liftIO $ createDirectoryIfMissing True $ obj "dir"
        liftIO $ setCurrentDirectory $ obj "dir"
        return ()

    obj "changedir" *> \out -> do
        () <- askOracle ()
        writeFile' out ""

    obj "pause.*" *> \out -> do
        liftIO $ sleep 0.1
        need $ obj "cdir" <.> takeExtension out
        writeFile' out ""

    obj "cdir.*" *> \out -> do
        pwd <- liftIO getCurrentDirectory
        let dir2 = obj $ "dir" ++ takeExtension out
        liftIO $ createDirectoryIfMissing True dir2
        liftIO $ setCurrentDirectory dir2
        liftIO $ sleep 0.2
        liftIO $ setCurrentDirectory pwd
        writeFile' out ""

    obj "createonce" *> \out -> do
        writeFile' out "X"

    obj "createtwice" *> \out -> do
        need [obj "createonce"]
        liftIO sleepFileTime
        writeFile' (obj "createonce") "Y"
        writeFile' out ""

    obj "listing" *> \out -> do
        writeFile' (out <.> "ls1") ""
        getDirectoryFiles (obj "") ["//*.ls*"]
        writeFile' (out <.> "ls2") ""
        writeFile' out ""

    obj "existance" *> \out -> do
        Development.Shake.doesFileExist $ obj "exists"
        writeFile' (obj "exists") ""
        writeFile' out ""

    obj "gen*" *> \out -> do
        writeFile' out out

    obj "needed1" *> \out -> do
        needed $ obj "gen1"
        writeFile' out ""

    obj "needed2" *> \out -> do
        orderOnly $ obj "gen2"
        needed $ obj "gen2"
        writeFile' out ""

    obj "tracker-write1" *> \out -> do
        () <- cmd "cmd /c" ["echo x > " ++ out <.> "txt"]
        need $ out <.> "txt"
        writeFile' out ""

    obj "tracker-write2" *> \out -> do
        () <- cmd "cmd /c" ["echo x > " ++ out <.> "txt"]
        writeFile' out ""

    obj "tracker-source2" *> \out -> copyFile' (obj "tracker-source1") out
    obj "tracker-read1" *> \out -> do
        () <- cmd "cmd /c" ["type " ++ toNative (obj "tracker-source1") ++ " > nul"]
        writeFile' out ""
    obj "tracker-read2" *> \out -> do
        () <- cmd "cmd /c" ["type " ++ toNative (obj "tracker-source1") ++ " > nul"]
        need $ obj "tracker-source1"
        writeFile' out ""
    obj "tracker-read3" *> \out -> do
        () <- cmd "cmd /c" ["type " ++ toNative (obj "tracker-source2") ++ " > nul"]
        need $ obj "tracker-source2"
        writeFile' out ""


test build obj = do
    dir <- getCurrentDirectory
    let crash args parts = do
            assertException parts (build $ "--quiet" : args)
                `finally` setCurrentDirectory dir

    crash ["changedir"] ["current directory has changed"]
    build ["cdir.1","cdir.2","-j1"]
    build ["--clean","cdir.1","pause.2","-j1"]
    crash ["--clean","cdir.1","pause.2","-j2"] ["before building output/lint/","current directory has changed"]
    crash ["existance"] ["changed since being depended upon"]
    crash ["createtwice"] ["changed since being depended upon"]
    crash ["listing"] ["changed since being depended upon","output/lint"]
    crash ["--clean","listing","existance"] ["changed since being depended upon"]
    crash ["needed1"] ["'needed' file required rebuilding"]
    build ["needed2"]

    tracker <- findExecutable "tracker.exe"
    when (isJust tracker) $ do
        writeFile (obj "tracker-source1") ""
        writeFile (obj "tracker-source2") ""
        crash ["tracker-write1"] ["not have its creation tracked","lint/tracker-write1","lint/tracker-write1.txt"]
        build ["tracker-write2"]
        crash ["tracker-read1"] ["used but not depended upon","lint/tracker-source1"]
        build ["tracker-read2"]
        crash ["tracker-read3"] ["depended upon after being used","lint/tracker-source2"]
