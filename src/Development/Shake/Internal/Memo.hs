{-# LANGUAGE LambdaCase #-}

module Development.Shake.Internal.Memo(
    memoFiles, memoFiles', hashState,

    fsMemoRestore, fsMemoSave,
    ) where

import Control.DeepSeq (force)
import Control.Exception.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe
import Data.Traversable
import System.Directory
import System.IO
import System.IO.Error
import Text.Read

import Development.Shake.FilePath
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.Options
import Development.Shake.Internal.Rules.Directory (getDirectoryFilesIO)
import Development.Shake.Internal.Value

-- | Cache files produced by an action using the persisitent store. The action
-- identifier must be unique to the action, as it's used as a key to indexing
-- into the cache.
--
-- The example below caches the output of @gcc@:
--
-- @
-- "*.o" %> \\f -> memoFiles "gcc" [f] $ cmd "gcc -c" [f -\<.\> "c"]
-- @
--
-- Alternatively, you can use the `Development.Shake.Command.Capture` option
-- for `Development.Shake.Command.cmd`:
--
-- @
-- "*.o" %> \\f -> cmd "gcc -c" [f -\<.\> "c"] (Capture [f])
-- @
memoFiles
  :: String -- ^ Action identifier
  -> [FilePattern] -- ^ Files to capture
  -> Action a
  -> Action (Maybe a)
memoFiles name = memoFiles' name (Just name)

memoFiles'
  :: String -- ^ action identifier
  -> Maybe String -- ^ short description
  -> [FilePattern] -- ^ files to capture
  -> Action a
  -> Action (Maybe a)
memoFiles' actName descr patterns act = do
    queryString <- stateString actName
    options <- Action $ getsRO globalOptions
    let shasum = SHA.showDigest $ SHA.sha256 $ LBS.fromStrict $ BS.pack queryString

    liftIO (shakeMemoRestore options shasum) >>= \case
        False -> do
            r <- act
            filesToCapture <- liftIO $ getDirectoryFilesIO "" patterns
            liftIO $ shakeMemoSave options shasum queryString filesToCapture
            putNormal $ "Saving to cache (" ++ shasum ++ ")"
            return $ Just r
        True -> do
            verb <- getVerbosity
            if verb < Loud
                then case descr of
                    Just d -> do
                        stack <- Action $ getsRW localStack
                        putNormal $ "# " ++ d ++
                            " (cached) (for " ++ showTopStack stack ++ ")"
                    _ -> return ()
                else putNormal $
                    "Using cache for " ++ actName ++ " (" ++ shasum ++ ")"
            return Nothing

ignoreErrors :: IO () -> IO ()
ignoreErrors a = a `catch` \e -> const (return ()) (e :: IOException)

-- | Hash the current state of the dependencies, together with the given string.
-- This function can be useful to write rules that don't depend on timestamps.
-- For example, instead of writing a rule that touches an empty file upon
-- completion, you could make it write the string returned by 'hashState'
-- to the file.
hashState :: String -> Action String
hashState givenString = SHA.showDigest . SHA.sha256 . LBS.pack <$>
    stateString givenString

stateString :: String -> Action String
stateString givenString = do
    database <- Action $ getsRO globalDatabase
    deps <- Action $ getsRW localDepends
    dependKeys <- liftIO $ concat <$> mapM (listDepends database) deps
    inputs <- forM dependKeys $ \key -> do
        hash <- getSummary key
        return $ show (typeKey key) ++ ": " ++ show key ++ ": " ++ hash
    return $ unlines $ givenString : inputs

getSummary :: Key -> Action String
getSummary key = do
    cacheRef <- Action $ getsRO globalSHACache
    cache <- liftIO $ readIORef cacheRef
    case Map.lookup key cache of
        Just r -> return r
        Nothing -> do
            rules <- Action $ getsRO globalRules
            database <- Action $ getsRO globalDatabase
            case Map.lookup (typeKey key) rules of
                Just builtinRule -> do
                    value <- liftIO $ lookupValue database key
                    summary <- liftIO $ builtinSummary builtinRule key value
                    liftIO $ atomicModifyIORef' cacheRef $ \cache' ->
                        ( Map.insert key summary cache', ())
                    return summary
                Nothing -> error $ "No builtin rule is found for " ++ show
                    (typeKey key)

hashFileIO :: FilePath -> IO String
hashFileIO path = do
    sha <- SHA.showDigest . SHA.sha256 <$> LBS.readFile path
    return $! force sha

-- | The 'Development.Shake.shakeMemoRestore' handler for the file system
-- backend.
fsMemoRestore :: FilePath -> String -> IO Bool{- success? -}
fsMemoRestore cacheDir shasum = fmap isJust $ runMaybeT $ do
    Right answer <- liftIO $ tryJust (guard . isDoesNotExistError) $
        readFile $ answerFile cacheDir shasum
    anss <- for (lines answer) $ \xs -> do
        (keyStr, _:val) <- return $ break (==':') xs
        Just key <- return $ readMaybe keyStr
        return (key, val)
    forM_ anss $ \(k, v) -> do
        liftIO $ createDirectoryIfMissing True $ takeDirectory k
        Right _ <- liftIO $ tryJust (guard . isDoesNotExistError) $ do
            copyFile (contentFile cacheDir v) k
            touchFile (contentFile cacheDir v)
        return ()
    liftIO $ touchFile $ answerFile cacheDir shasum

-- | The 'Development.Shake.shakeMemoSave' handler for the file system backend.
fsMemoSave :: FilePath -> String -> String -> [FilePattern] -> IO ()
fsMemoSave cacheDir shasum queryString filesToCapture = ignoreErrors $ do
    lns <- forM filesToCapture $ \file -> do
        hash <- hashFileIO file
        createDirectoryIfMissing True $ takeDirectory $ contentFile cacheDir hash
        c <- doesFileExist $ contentFile cacheDir hash
        unless c $ copyFileAtomic file $ contentFile cacheDir hash
        return (show file ++ ":" ++ hash)
    createDirectoryIfMissing True $ takeDirectory $ questionFile cacheDir shasum
    writeFileAtomic (questionFile cacheDir shasum) queryString
    writeFileAtomic (answerFile cacheDir shasum) $ unlines lns

touchFile :: FilePath -> IO ()
touchFile path = withFile path AppendMode $ \_ -> return ()

copyFileAtomic :: FilePath -> FilePath -> IO ()
copyFileAtomic sourcePath destPath = withNewFileAtomic destPath $ \hd ->
    LBS.hPutStr hd =<< LBS.readFile sourcePath

writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic destPath str = withNewFileAtomic destPath $ \h ->
    hPutStr h str

withNewFileAtomic :: FilePath -> (Handle -> IO ()) -> IO ()
withNewFileAtomic destPath writer = do
    withFile tempPath WriteMode writer
    renameFile tempPath destPath
  where
    tempPath = destPath <.> "tmp"

answerFile :: String -> String -> FilePath
answerFile basePath hs = basePath </> "qa" </> take 2 hs </> drop 2 hs <.> "a"

questionFile :: String -> String -> FilePath
questionFile basePath hs = basePath </> "qa" </> take 2 hs </> drop 2 hs <.> "q"

contentFile :: String -> String -> FilePath
contentFile basePath hs = basePath </> "content" </> take 2 hs </> drop 2 hs
