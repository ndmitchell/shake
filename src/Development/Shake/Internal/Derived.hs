
module Development.Shake.Internal.Derived(
    system', systemCwd, systemOutput,
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged,
    withTempFile, withTempDir,
    getHashedShakeVersion,
    par, forP
    ) where

import Control.Applicative
import Control.Exception.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.Process
import System.Directory
import System.Exit
import System.IO.Extra hiding (withTempFile, withTempDir, readFile')

import Development.Shake.Internal.Core
import Development.Shake.Internal.Rules.File
import Development.Shake.FilePath
import Development.Shake.Internal.Types
import qualified Data.ByteString as BS
import Data.Hashable
import Prelude


-- | Get a checksum of a list of files, suitable for using as `shakeVersion`.
--   This will trigger a rebuild when the Shake rules defined in any of the files are changed.
--   For example:
--
-- @
-- main = do
--     ver <- 'getHashedShakeVersion' [\"Shakefile.hs\"]
--     'shakeArgs' 'shakeOptions'{'shakeVersion' = ver} ...
-- @
--
--   To automatically detect the name of the current file, turn on the @TemplateHaskell@
--   extension and write @$(LitE . StringL . loc_filename \<$\> location)@.
--
--   This feature can be turned off during development by passing
--   the flag @--no-rule-version@ or setting 'shakeVersionIgnore' to 'True'.
getHashedShakeVersion :: [FilePath] -> IO String
getHashedShakeVersion files = do
    hashes <- mapM (fmap (hashWithSalt 0) . BS.readFile) files
    return $ "hash-" ++ show (hashWithSalt 0 hashes)


checkExitCode :: String -> ExitCode -> Action ()
checkExitCode _ ExitSuccess = return ()
checkExitCode cmd (ExitFailure i) = liftIO $ errorIO $ "System command failed (code " ++ show i ++ "):\n" ++ cmd

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
    need [old]
    putLoud $ "Copying from " ++ old ++ " to " ++ new
    liftIO $ copyFile old new


-- | @copyFileChanged old new@ copies the existing file from @old@ to @new@, if the contents have changed.
--   The @old@ file will be tracked as a dependency.
copyFileChanged :: FilePath -> FilePath -> Action ()
copyFileChanged old new = do
    need [old]
    -- in newer versions of the directory package we can use copyFileWithMetadata which (we think) updates
    -- the timestamp as well and thus no need to read the source file twice.
    unlessM (liftIO $ doesFileExist new &&^ fileEq old new) $ do
        putLoud $ "Copying from " ++ old ++ " to " ++ new
        -- copyFile does a lot of clever stuff with permissions etc, so make sure we just reuse it
        liftIO $ copyFile old new


-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
readFile' :: FilePath -> Action String
readFile' x = need [x] >> liftIO (readFile x)

-- | Write a file, lifted to the 'Action' monad.
writeFile' :: MonadIO m => FilePath -> String -> m ()
writeFile' name x = liftIO $ writeFile name x


-- | A version of 'readFile'' which also splits the result into lines.
--   The argument file will be tracked as a dependency.
readFileLines :: FilePath -> Action [String]
readFileLines = fmap lines . readFile'

-- | A version of 'writeFile'' which writes out a list of lines.
writeFileLines :: MonadIO m => FilePath -> [String] -> m ()
writeFileLines name = writeFile' name . unlines


-- | Write a file, but only if the contents would change.
writeFileChanged :: MonadIO m => FilePath -> String -> m ()
writeFileChanged name x = liftIO $ do
    b <- doesFileExist name
    if not b then writeFile name x else do
        -- Cannot use ByteString here, since it has different line handling
        -- semantics on Windows
        b <- withFile name ReadMode $ \h -> do
            src <- hGetContents h
            return $! src /= x
        when b $ writeFile name x


-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes (provided the file is not still open).
--   The 'FilePath' will not have any file extension, will exist, and will be zero bytes long.
--   If you require a file with a specific name, use 'withTempDir'.
withTempFile :: (FilePath -> Action a) -> Action a
withTempFile act = do
    (file, del) <- liftIO newTempFile
    act file `actionFinally` del


-- | Create a temporary directory inside the system temporary directory.
--   The directory will be deleted after the action completes.
withTempDir :: (FilePath -> Action a) -> Action a
withTempDir act = do
    (dir,del) <- liftIO newTempDir
    act dir `actionFinally` del


-- | A 'parallel' version of 'forM'.
forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = parallel $ map f xs

-- | Execute two operations in parallel, based on 'parallel'.
par :: Action a -> Action b -> Action (a,b)
par a b = do [Left a, Right b] <- parallel [Left <$> a, Right <$> b]; return (a,b)
