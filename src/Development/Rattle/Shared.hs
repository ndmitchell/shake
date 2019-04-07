{-# LANGUAGE ViewPatterns #-}

module Development.Rattle.Shared(
    Shared, withShared,
    getSpeculate, setSpeculate,
    getFile, setFile,
    getCmdTraces, addCmdTrace
    ) where

import General.Extra
import Development.Rattle.Types
import Development.Rattle.Hash
import System.FilePath
import System.Directory
import System.IO.Extra
import Data.Maybe
import Control.Monad.Extra
import Text.Read
import Control.Concurrent.Extra


data Shared = Shared Lock FilePath

withShared :: FilePath -> (Shared -> IO a) -> IO a
withShared dir act = do
    lock <- newLock
    createDirectoryRecursive dir
    act $ Shared lock dir

filename :: Hash -> String
filename (fromHash -> a:b:cs) = [a,b] </> cs

getList :: (Show a, Read b) => String -> Shared -> a -> IO [b]
getList typ (Shared lock dir) name = withLock lock $ do
    let file = dir </> typ </> filename (hashString $ show name)
    b <- doesFileExist file
    if not b then return [] else mapMaybe readMaybe . lines <$> readFileUTF8' file

setList :: (Show a, Show b) => String -> IOMode -> Shared -> a -> [b] -> IO ()
setList typ mode (Shared lock dir) name vals = withLock lock $ do
    let file = dir </> typ </> filename (hashString $ show name)
    createDirectoryRecursive $ takeDirectory file
    unlessM (doesFileExist $ file <.> "txt") $
        writeFile (file <.> "txt") $ show name
    withFile file mode $ \h -> do
        hSetEncoding h utf8
        hPutStr h $ unlines $ map show vals


getSpeculate :: Shared -> String -> IO [Cmd]
getSpeculate = getList "speculate"

setSpeculate :: Shared -> String -> [Cmd] -> IO ()
setSpeculate = setList "speculate" WriteMode

getCmdTraces :: Shared -> Cmd -> IO [Trace Hash]
getCmdTraces = getList "command"

addCmdTrace :: Shared -> Cmd -> Trace Hash -> IO ()
addCmdTrace share cmd t = setList "command" AppendMode share cmd [t]


getFile :: Shared -> Hash -> IO (Maybe (FilePath -> IO ()))
getFile (Shared lock dir) hash = do
    let file = dir </> "files" </> filename hash
    b <- doesFileExist file
    return $ if not b then Nothing else Just $ copyFile file

setFile :: Shared -> FilePath -> Hash -> IO Bool -> IO ()
setFile (Shared lock dir) source hash check = do
    let file = dir </> "files" </> filename hash
    b <- doesFileExist file
    unlessM (doesFileExist file) $ withLock lock $ do
        createDirectoryRecursive $ takeDirectory file
        copyFile source (file <.> "tmp")
        good <- check
        if not good then
            removeFile $ file <.> "tmp"
         else
            renameFile (file <.> "tmp") file
