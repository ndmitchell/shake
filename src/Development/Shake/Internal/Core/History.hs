{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Internal.Core.History(
    History, newHistory, addHistory,
    hasHistory
    ) where

import Development.Shake.Internal.Value
import Development.Shake.Classes
import General.Binary
import General.Extra
import General.Chunks
import Control.Monad.Extra
import System.FilePath
import System.Directory
import System.IO
import Numeric
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.FileName
import Data.Monoid
import qualified Data.ByteString as BS

{-
#ifndef mingw32_HOST_OS
import System.Posix.Files(createLink)
#else

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateHardLinkW" c_CreateHardLinkW :: Ptr CWchar -> Ptr CWchar -> Ptr () -> IO Bool

createLink :: FilePath -> FilePath -> IO ()
createLink from to = withCWString from $ \cfrom -> withCWString to $ \cto -> do
    res <- c_CreateHardLinkW cfrom cto nullPtr
    unless res $ error $ show ("Failed to createLink", from, to)

#endif
-}


data History = History
    {keyOp :: BinaryOp Key
    ,historyRoot :: FilePath
    }

newHistory :: BinaryOp Key -> FilePath -> IO History
newHistory keyOp historyRoot = return History{..}


data Entry = Entry
    {entryKey :: Key
    ,entryDepends :: [[(Key, BS.ByteString)]]
    ,entryResult :: BS.ByteString
    ,entryFiles :: [(FilePath, FileHash)]
    } deriving (Show, Eq)

putEntry :: BinaryOp Key -> Entry -> Builder
putEntry binop Entry{..} =
    putExN (putOp binop entryKey) <>
    putExN (putExList $ map (putExList . map putDepend) entryDepends) <>
    putExN (putExList $ map putFile entryFiles) <>
    putEx entryResult
    where
        putDepend (a,b) = putExN (putOp binop a) <> putEx b
        putFile (a,b) = putExStorable b <> putEx a

getEntry :: BinaryOp Key -> BS.ByteString -> Entry
getEntry binop x
    | (x1, x) <- getExN x
    , (x2, x) <- getExN x
    , (x3, x4) <- getExN x
    = Entry
        {entryKey = getOp binop x1
        ,entryDepends = map (map getDepend . getExList) $ getExList x2
        ,entryFiles = map getFile $ getExList x3
        ,entryResult = getEx x4
        }
    where
        getDepend x | (a, b) <- getExN x = (getOp binop a, getEx b)
        getFile x | (b, a) <- binarySplit x = (getEx a, b)

historyFileDir :: History -> Key -> FilePath
historyFileDir history key = historyRoot history </> ".shake.cache" </> showHex (abs $ hash key) ""

loadHistoryEntry :: History -> Key -> IO [Entry]
loadHistoryEntry history key = do
    let file = historyFileDir history key </> "_key"
    b <- doesFileExist_ file
    if not b then return [] else do
        (items, slop) <- withFile file ReadMode $ \h ->
            readChunksDirect h maxBound
        unless (BS.null slop) $
            error $ "Corrupted key file, " ++ show file
        return $ filter (\e -> entryKey e == key) $ map (getEntry $ keyOp history) items

hasHistory :: History -> Key -> IO Bool
hasHistory history key = not . null <$> loadHistoryEntry history key

saveHistoryEntry :: History -> Entry -> IO ()
saveHistoryEntry history entry = do
    let dir = historyFileDir history (entryKey entry)
    createDirectoryRecursive dir
    withFile (dir </> "_key") AppendMode $ \h -> writeChunkDirect h $ putEntry (keyOp history) entry
    forM_ (entryFiles entry) $ \(file, hash) ->
        -- FIXME: should use a combination of symlinks and making files read-only
        unlessM (doesFileExist_ $ dir </> show hash) $
            copyFile file (dir </> show hash)


addHistory :: History -> Key -> [[(Key, BS.ByteString)]] -> BS.ByteString -> [FilePath] -> IO ()
addHistory history entryKey entryDepends entryResult files = do
    hashes <- mapM (getFileHash . fileNameFromString) files
    saveHistoryEntry history Entry{entryFiles = zip files hashes, ..}