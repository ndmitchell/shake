{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Test.Lint(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import Control.Exception hiding (assert)
import Control.Monad
import Data.List
import System.Directory as IO


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
        need [obj "cdir" <.> takeExtension out]
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

    obj "existance" *> \out -> do
        Development.Shake.doesFileExist $ obj "exists"
        writeFile' (obj "exists") ""
        writeFile' out ""

test build obj = do
    dir <- getCurrentDirectory
    let crash args parts = do
            res <- try $ build $ "--quiet" : args
            setCurrentDirectory dir
            case res of
                Left (err :: SomeException) -> let s = show err in forM_ parts $ \p ->
                    assert (p `isInfixOf` s) $ "Incorrect exception, missing part:\nGOT: " ++ s ++ "\nWANTED: " ++ p
                Right _ -> error "Expected an exception but succeeded"

    crash ["changedir"] ["current directory has changed"]
    build ["cdir.1","cdir.2","-j1"]
    build ["--clean","cdir.1","pause.2","-j1"]
    crash ["--clean","cdir.1","pause.2","-j2"] ["before building output/lint/","current directory has changed"]
    crash ["existance"] ["changed since being built"]
    crash ["createtwice"] ["changed since being built"]
