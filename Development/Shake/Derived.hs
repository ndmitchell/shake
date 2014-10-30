
module Development.Shake.Derived(
    system', systemCwd, systemOutput,
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Process
import System.Directory
import System.Exit
import System.IO

import Development.Shake.Core
import Development.Shake.Rules.File
import Development.Shake.FilePath
import Development.Shake.Types
import General.Base
import qualified Data.ByteString as BS


checkExitCode :: String -> ExitCode -> Action ()
checkExitCode cmd ExitSuccess = return ()
checkExitCode cmd (ExitFailure i) = error $ "System command failed (code " ++ show i ++ "):\n" ++ cmd

{-# DEPRECATED system' "Use 'command' or 'cmd'" #-}
{-# DEPRECATED systemCwd "Use 'command' or 'cmd' with 'Cwd'" #-}
{-# DEPRECATED systemOutput "Use 'command' or 'cmd' with 'Stdout' or 'Stderr'" #-}

-- | /Deprecated:/ Please use 'command' or 'cmd' instead.
--   This function will be removed in a future version.
--
--   Execute a system command. This function will raise an error if the exit code is non-zero.
--   Before running 'system'' make sure you 'need' any required files.
system' :: FilePath -> [String] -> Action ()
system' path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    v <- getVerbosity
    putLoud cmd
    res <- (if v >= Loud then quietly else id) $ traced (takeBaseName path) $ rawSystem path2 args
    checkExitCode cmd res


-- | /Deprecated:/ Please use 'command' or 'cmd' instead, with 'Cwd'.
--   This function will be removed in a future version.
--
--   Execute a system command with a specified current working directory (first argument).
--   This function will raise an error if the exit code is non-zero.
--   Before running 'systemCwd' make sure you 'need' any required files.
--
-- @
-- 'systemCwd' \"\/usr\/MyDirectory\" \"pwd\" []
-- @
systemCwd :: FilePath -> FilePath -> [String] -> Action ()
systemCwd cwd path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    res <- traced (takeBaseName path) $ do
        -- FIXME: Should I be using the non-exported System.Process.syncProcess?
        --        That installs/removes signal handlers.
        hdl <- runProcess path2 args (Just cwd) Nothing Nothing Nothing Nothing
        waitForProcess hdl
    checkExitCode cmd res


-- | /Deprecated:/ Please use 'command' or 'cmd' instead, with 'Stdout' or 'Stderr'.
--   This function will be removed in a future version.
--
--   Execute a system command, returning @(stdout,stderr)@.
--   This function will raise an error if the exit code is non-zero.
--   Before running 'systemOutput' make sure you 'need' any required files.
systemOutput :: FilePath -> [String] -> Action (String, String)
systemOutput path args = do
    let path2 = toNative path
    let cmd = unwords $ path2 : args
    putLoud cmd
    (res,stdout,stderr) <- traced (takeBaseName path) $ readProcessWithExitCode path2 args ""
    checkExitCode cmd res
    return (stdout, stderr)


-- | @copyFile' old new@ copies the existing file from @old@ to @new@.
--   The @old@ file will be tracked as a dependency.
copyFile' :: FilePath -> FilePath -> Action ()
copyFile' old new = do
    need old
    putLoud $ "Copying from " ++ old ++ " to " ++ new
    liftIO $ copyFile old new


-- | @copyFile' old new@ copies the existing file from @old@ to @new@, if the contents have changed.
--   The @old@ file will be tracked as a dependency.
copyFileChanged :: FilePath -> FilePath -> Action ()
copyFileChanged old new = do
    need old
    eq <- liftIO $ doesFileExist new &&^ do
        withBinaryFile old ReadMode $ \h1 -> withBinaryFile new ReadMode $ \h2 ->
            liftM2 (==) (hFileSize h1) (hFileSize h2) &&^
                liftM2 (==) (BS.hGetContents h1) (BS.hGetContents h2)
    when (not eq) $ do
        putLoud $ "Copying from " ++ old ++ " to " ++ new
        -- copyFile does a lot of clever stuff with permissions etc, so make sure we just reuse it
        liftIO $ copyFile old new


-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
readFile' :: FilePath -> Action String
readFile' x = need x >> liftIO (readFile x)

-- | Write a file, lifted to the 'Action' monad.
writeFile' :: FilePath -> String -> Action ()
writeFile' name x = liftIO $ writeFile name x


-- | A version of 'readFile'' which also splits the result into lines.
--   The argument file will be tracked as a dependency.
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
        -- Cannot use ByteString here, since it has different line handling
        -- semantics on Windows
        b <- withFile name ReadMode $ \h -> do
            src <- hGetContents h
            return $! src /= x
        when b $ writeFile name x
