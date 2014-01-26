
module Examples.Test.Config(main) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Examples.Util
import Data.Char
import Data.Maybe
import System.Directory


main = shaken test $ \args obj -> do
    want $ map obj ["hsflags.var","cflags.var","none.var"]
    usingConfigFile $ obj "config"
    obj "*.var" *> \out -> do
        cfg <- getConfig $ map toUpper $ takeBaseName out
        liftIO $ appendFile (out -<.> "times") "X"
        writeFile' out $ fromMaybe "" cfg


test build obj = do
    build ["clean"]
    createDirectoryIfMissing True $ obj ""
    writeFile (obj "config") $ unlines
        ["HEADERS_DIR = /path/to/dir"
        ,"CFLAGS = -O2 -I${HEADERS_DIR} -g"
        ,"HSFLAGS = -O2"]
    build []
    assertContents (obj "cflags.var") "-O2 -I/path/to/dir -g"
    assertContents (obj "hsflags.var") "-O2"
    assertContents (obj "none.var") ""

    appendFile (obj "config") $ unlines
        ["CFLAGS = $CFLAGS -w"]
    build []
    assertContents (obj "cflags.var") "-O2 -I/path/to/dir -g -w"
    assertContents (obj "hsflags.var") "-O2"
    assertContents (obj "cflags.times") "XX"
    assertContents (obj "hsflags.times") "X"
    assertContents (obj "none.times") "X"
