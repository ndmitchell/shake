
module Development.Shake.Derived where

import Control.Monad
import Control.Monad.IO.Class
import System.Process
import System.Directory
import System.Exit
import qualified Data.ByteString.Char8 as BS

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePath


-- | Execute a system command. This function will raise an error if the exit code is non-zero.
--   Before running 'system'' make sure you 'need' any required files.
system' :: FilePath -> [String] -> Action ()
system' path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    res <- traced ("system " ++ cmd) $ rawSystem path2 args
    when (res /= ExitSuccess) $
        error $ "System command failed:\n" ++ cmd

-- | Execute a system command, returning @(stdout,stderr)@.
--   This function will raise an error if the exit code is non-zero.
--   Before running 'systemOutput'' make sure you 'need' any required files.
systemOutput :: FilePath -> [String] -> Action (String, String)
systemOutput path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    (res,stdout,stderr) <- traced ("system' " ++ cmd) $ readProcessWithExitCode path2 args ""
    when (res /= ExitSuccess) $
        error $ "System command failed:\n" ++ cmd
    return (stdout, stderr)


-- | @copyFile old new@ copies the existing file from @old@ to @new@. The @old@ file is has 'need' called on it
--   before copying the file.
copyFile' :: FilePath -> FilePath -> Action ()
copyFile' old new = need [old] >> liftIO (copyFile old new)


-- | Read a file, after calling 'need'.
readFile' :: FilePath -> Action String
readFile' x = need [x] >> liftIO (readFile x)

-- | Write a file, lifted to the 'Action' monad.
writeFile' :: FilePath -> String -> Action ()
writeFile' name x = liftIO $ writeFile name x


-- | A version of 'readFile'' which also splits the result into lines.
readFileLines :: FilePath -> Action [String]
readFileLines = fmap lines . readFile'

-- | A version of 'writeFile'' which writes out a list of lines.
writeFileLines :: FilePath -> [String] -> Action ()
writeFileLines name = writeFile' name . unlines


-- | Write a file, but only if the contents would change.
writeFileChanged :: FilePath -> String -> Action ()
writeFileChanged name x = liftIO $ do
    b <- doesFileExist name
    if not b then writeFile name x else do
        orig <- BS.readFile name
        let new = BS.pack x
        when (orig /= new) $ BS.writeFile name new
