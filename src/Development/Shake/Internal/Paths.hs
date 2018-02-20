
-- | The information from Paths_shake cleaned up
module Development.Shake.Internal.Paths(
    shakeVersionString,
    initDataDirectory,
    hasManualData, copyManualData,
    readDataFileHTML
    ) where

import Paths_shake
import Control.Monad
import Data.Version
import System.Directory
import System.FilePath
import System.Info.Extra
import System.IO.Error
import General.Extra
import qualified Data.ByteString.Lazy as LBS


shakeVersionString :: String
shakeVersionString = showVersion version

-- The data files may be located relative to the current directory, if so cache it in advance
initDataDirectory :: IO ()
-- my debug getDataFileName (in Paths) uses a cache of the Cwd
-- make sure we force the cache before changing directory
initDataDirectory = void $ getDataFileName ""


readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = do
    file <- getDataFileName $ "html" </> file
    LBS.readFile file


hasManualData :: IO Bool
hasManualData = do
    manual <- getDataFileName "docs/manual"
    doesDirectoryExist manual `catchIOError` const (return False)

copyManualData :: FilePath -> IO ()
copyManualData dest = do
    createDirectoryRecursive dest
    manual <- getDataFileName "docs/manual"
    forM_ ["Build.hs","main.c","constants.c","constants.h","build" <.> if isWindows then "bat" else "sh"] $ \file ->
        copyFile (manual </> file) (dest </> file)
