
module Development.Shake.Derived where

import Control.Monad
import Control.Monad.IO.Class
import System.Cmd
import System.Directory
import System.Exit

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePath


-- | Execute a system command. This function does not do any escaping of 
system' :: FilePath -> [String] -> Action ()
system' path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    res <- liftIO $ rawSystem path2 args
    when (res /= ExitSuccess) $ do
        k <- currentRule
        error $ "System command failed while building " ++ show k ++ ", " ++ cmd


copyFile' :: FilePath -> FilePath -> Action ()
copyFile' old new = need [old] >> liftIO (copyFile old new)


readFile' :: FilePath -> Action String
readFile' x = need [x] >> liftIO (readFile x)

writeFile' :: FilePath -> String -> Action ()
writeFile' name x = liftIO $ writeFile name x


readFileLines :: FilePath -> Action [String]
readFileLines = fmap lines . readFile'

writeFileLines :: FilePath -> [String] -> Action ()
writeFileLines name = writeFile' name . unlines
