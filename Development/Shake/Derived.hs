
module Development.Shake.Derived where

import Control.Monad
import Control.Monad.IO.Class
import System.Cmd
import System.Exit

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePath


system_ :: [String] -> Action ()
system_ (x:xs) = do
    let cmd = unwords $ toNative x : xs
    putLoud cmd
    res <- liftIO $ system cmd
    when (res /= ExitSuccess) $ do
        k <- currentRule
        error $ "System command failed while building " ++ show k ++ ", " ++ cmd


readFile_ :: FilePath -> Action String
readFile_ x = need [x] >> liftIO (readFile x)

writeFile_ :: FilePath -> String -> Action ()
writeFile_ name x = liftIO $ writeFile name x


readFileLines :: FilePath -> Action [String]
readFileLines = fmap lines . readFile_

writeFileLines :: FilePath -> [String] -> Action ()
writeFileLines name = writeFile_ name . unlines
