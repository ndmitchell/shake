
module Development.Shake.Derived where

import Control.Monad
import Control.Monad.IO.Class
import System.Cmd
import System.Directory
import System.Exit

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePath


system' :: [String] -> Action ()
system' (x:xs) = do
    let cmd = unwords $ toNative x : xs
    putLoud cmd
    res <- liftIO $ system cmd
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
