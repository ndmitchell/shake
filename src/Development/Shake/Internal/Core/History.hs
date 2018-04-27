{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.Core.History(
    History, newHistory, addHistory, lookupHistory
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
import Development.Shake.Internal.Core.Wait3
import Development.Shake.Internal.FileName
import Data.Monoid
import Data.Functor
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.ByteString as BS
import Prelude

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
    {historyVersion :: Int
    ,keyOp :: BinaryOp Key
    ,historyRoot :: FilePath
    }

newHistory :: Int -> BinaryOp Key -> FilePath -> IO History
newHistory historyVersion keyOp historyRoot = return History{..}


data Entry = Entry
    {entryKey :: Key
    ,entryGlobalVersion :: !Int
    ,entryLocalVersion :: !Int
    ,entryDepends :: [[(Key, BS.ByteString)]]
    ,entryResult :: BS.ByteString
    ,entryFiles :: [(FilePath, FileHash)]
    } deriving (Show, Eq)

putEntry :: BinaryOp Key -> Entry -> Builder
putEntry binop Entry{..} =
    putEx entryGlobalVersion <>
    putEx entryLocalVersion <>
    putExN (putOp binop entryKey) <>
    putExN (putExList $ map (putExList . map putDepend) entryDepends) <>
    putExN (putExList $ map putFile entryFiles) <>
    putEx entryResult
    where
        putDepend (a,b) = putExN (putOp binop a) <> putEx b
        putFile (a,b) = putExStorable b <> putEx a

getEntry :: BinaryOp Key -> BS.ByteString -> Entry
getEntry binop x
    | (x1, x2, x) <- binarySplit2 x
    , (x3, x) <- getExN x
    , (x4, x) <- getExN x
    , (x5, x6) <- getExN x
    = Entry
        {entryGlobalVersion = x1
        ,entryLocalVersion = x2
        ,entryKey = getOp binop x3
        ,entryDepends = map (map getDepend . getExList) $ getExList x4
        ,entryFiles = map getFile $ getExList x5
        ,entryResult = getEx x6
        }
    where
        getDepend x | (a, b) <- getExN x = (getOp binop a, getEx b)
        getFile x | (b, a) <- binarySplit x = (getEx a, b)

historyFileDir :: History -> Key -> FilePath
historyFileDir history key = historyRoot history </> ".shake.cache" </> showHex (abs $ hash key) ""

loadHistoryEntry :: History -> Key -> Int -> IO [Entry]
loadHistoryEntry history key ver = do
    let file = historyFileDir history key </> "_key"
    b <- doesFileExist_ file
    if not b then return [] else do
        (items, slop) <- withFile file ReadMode $ \h ->
            readChunksDirect h maxBound
        unless (BS.null slop) $
            error $ "Corrupted key file, " ++ show file
        let eq Entry{..} = entryKey == key && entryGlobalVersion == historyVersion history && entryLocalVersion == ver
        return $ filter eq $ map (getEntry $ keyOp history) items


-- | Given a way to get the identity, see if you can a stored cloud version
lookupHistory :: History -> (Key -> Locked (Wait (Maybe BS.ByteString))) -> Key -> Int -> Locked (Wait (Maybe (BS.ByteString, [[Key]], IO ())))
lookupHistory history ask key ver = do
    ents <- liftIO $ loadHistoryEntry history key ver
    firstJustWaitUnordered $ flip map ents $ \Entry{..} -> do
        -- use Nothing to indicate success, Just () to bail out early on mismatch
        let result x = if isJust x then Nothing else Just $ (entryResult, map (map fst) entryDepends, ) $ do
                let dir = historyFileDir history entryKey
                forM_ entryFiles $ \(file, hash) -> do
                    createDirectoryRecursive $ takeDirectory file
                    copyFile (dir </> show hash) file
        fmap result <$> firstJustWaitOrdered
            [ firstJustWaitUnordered
                [ fmap test <$> ask k | (k, i1) <- kis
                , let test = maybe (Just ()) (\i2 -> if i1 == i2 then Nothing else Just ())]
            | kis <- entryDepends]


saveHistoryEntry :: History -> Entry -> IO ()
saveHistoryEntry history entry = do
    let dir = historyFileDir history (entryKey entry)
    createDirectoryRecursive dir
    withFile (dir </> "_key") AppendMode $ \h -> writeChunkDirect h $ putEntry (keyOp history) entry
    forM_ (entryFiles entry) $ \(file, hash) ->
        -- FIXME: should use a combination of symlinks and making files read-only
        unlessM (doesFileExist_ $ dir </> show hash) $
            copyFile file (dir </> show hash)


addHistory :: History -> Key -> Int -> [[(Key, BS.ByteString)]] -> BS.ByteString -> [FilePath] -> IO ()
addHistory history entryKey entryLocalVersion entryDepends entryResult files = do
    hashes <- mapM (getFileHash . fileNameFromString) files
    saveHistoryEntry history Entry{entryFiles = zip files hashes, entryGlobalVersion = historyVersion history, ..}
