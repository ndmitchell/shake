{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.History.Shared(
    Shared, newShared, SharedCache(..), SharedFileSystem(..),
    mkSharedCache, localFileSystem
    ) where

import Data.IORef
import Development.Shake.Internal.Value
import Development.Shake.Internal.History.Shared.Api
import Development.Shake.Internal.History.Symlink
import Development.Shake.Internal.History.Types
import Development.Shake.Classes
import General.Binary
import General.Extra
import General.Chunks
import Control.Monad.Extra
import System.Directory.Extra
import System.FilePath
import System.IO
import Numeric
import Development.Shake.Internal.FileInfo
import General.Wait
import Development.Shake.Internal.FileName
import Data.Monoid
import Data.Functor
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.ByteString as BS
import Prelude


data Shared = Shared
    {globalVersion :: !Ver
    ,keyOp :: BinaryOp Key
    ,sharedRoot :: FilePath
    }

newShared :: BinaryOp Key -> Ver -> FilePath -> IO Shared
newShared keyOp globalVersion sharedRoot = return Shared{..}


data Entry = Entry
    {entryKey :: Key
    ,entryGlobalVersion :: !Ver
    ,entryBuiltinVersion :: !Ver
    ,entryUserVersion :: !Ver
    ,entryDepends :: [[(Key, BS_Identity)]]
    ,entryResult :: BS_Store
    ,entryFiles :: [(FilePath, FileHash)]
    } deriving (Show, Eq)

putEntry :: BinaryOp Key -> Entry -> Builder
putEntry binop Entry{..} =
    putExStorable entryGlobalVersion <>
    putExStorable entryBuiltinVersion <>
    putExStorable entryUserVersion <>
    putExN (putOp binop entryKey) <>
    putExN (putExList $ map (putExList . map putDepend) entryDepends) <>
    putExN (putExList $ map putFile entryFiles) <>
    putEx entryResult
    where
        putDepend (a,b) = putExN (putOp binop a) <> putEx b
        putFile (a,b) = putExStorable b <> putEx a

getEntry :: BinaryOp Key -> BS.ByteString -> Entry
getEntry binop x
    | (x1, x2, x3, x) <- binarySplit3 x
    , (x4, x) <- getExN x
    , (x5, x) <- getExN x
    , (x6, x7) <- getExN x
    = Entry
        {entryGlobalVersion = x1
        ,entryBuiltinVersion = x2
        ,entryUserVersion = x3
        ,entryKey = getOp binop x4
        ,entryDepends = map (map getDepend . getExList) $ getExList x5
        ,entryFiles = map getFile $ getExList x6
        ,entryResult = getEx x7
        }
    where
        getDepend x | (a, b) <- getExN x = (getOp binop a, getEx b)
        getFile x | (b, a) <- binarySplit x = (getEx a, b)

sharedFileDir :: Shared -> Key -> FilePath
sharedFileDir shared key = sharedRoot shared </> ".shake.cache" </> showHex (abs $ hash key) ""

mkSharedCache :: SharedFileSystem Shared -> SharedCache Shared
mkSharedCache SharedFileSystem{..} = SharedCache{..} where
  addShared shared@Shared{..} entryKey entryBuiltinVersion entryUserVersion entryDepends entryResult files = do
    files <- mapM (\x -> (x,) <$> getFileHash (fileNameFromString x)) files
    saveSharedEntry shared entryKey files $ putEntry keyOp $ Entry{entryFiles = files, entryGlobalVersion = globalVersion, ..}

  lookupShared shared@Shared{..} ask key builtinVersion userVersion = do
    let eq Entry{..} = entryKey == key && entryGlobalVersion == globalVersion && entryBuiltinVersion == builtinVersion && entryUserVersion == userVersion
    ents <- liftIO $ filter eq . map (getEntry keyOp) <$> loadSharedEntry shared key
    flip firstJustWaitUnordered ents $ \Entry{..} -> do
        -- use Nothing to indicate success, Just () to bail out early on mismatch
        let result x = if isJust x then Nothing else Just $ (entryResult, map (map fst) entryDepends, ) $ do
                let dir = sharedFileDir shared entryKey
                forM_ entryFiles $ \(file, hash) ->
                    copyFileLink (dir </> show hash) file
        result <$> firstJustM id
            [ firstJustWaitUnordered id
                [ test <$> ask k | (k, i1) <- kis
                , let test = maybe (Just ()) (\i2 -> if i1 == i2 then Nothing else Just ())]
            | kis <- entryDepends]

  removeShared shared f = do
    deletedRef <- newIORef 0
    updateSharedKeys shared $ \k -> do
      let del = f k
      when del $ atomicModifyIORef' deletedRef $ \dd -> (dd+1, ())
      return del
    deleted <- readIORef deletedRef
    liftIO $ putStrLn $ "Deleted " ++ show deleted ++ " entries"

  listShared :: Shared -> IO ()
  listShared (shared :: Shared) = updateSharedKeys shared $ \key -> do
    items <- loadSharedEntry shared key
    forM_ items $ \item -> do
      let Entry{..} = getEntry (keyOp shared) item
      putStrLn $ "  Key: " ++ show entryKey
      forM_ entryFiles $ \(file,_) ->
        putStrLn $ "    File: " ++ file
    return True

-- | The default implementation. Uses the local file system
localFileSystem :: SharedFileSystem Shared
localFileSystem = SharedFileSystem{..} where
  loadSharedEntry shared@Shared{..} key = do
    let file = sharedFileDir shared key </> "_key"
    b <- doesFileExist_ file
    if not b then return [] else do
        (items, slop) <- withFile file ReadMode $ \h ->
            readChunksDirect h maxBound
        unless (BS.null slop) $
            error $ "Corrupted key file, " ++ show file
        return items
  saveSharedEntry shared entryKey files entry = do
    let dir = sharedFileDir shared entryKey
    createDirectoryRecursive dir
    withFile (dir </> "_key") AppendMode $ \h -> writeChunkDirect h entry
    forM_ files $ \(file, hash) ->
        unlessM (doesFileExist_ $ dir </> show hash) $
            copyFileLink file (dir </> show hash)
  sharedFileDir shared key = sharedRoot shared </> ".shake.cache" </> showHex (abs $ hash key) ""

  updateSharedKeys Shared{..} test = do
    dirs <- listDirectories $ sharedRoot </> ".shake.cache"
    forM_ dirs $ \dir -> do
        (items, _slop) <- withFile (dir </> "_key") ReadMode $ \h ->
            readChunksDirect h maxBound
        -- if any key matches, clean them all out
        b <- anyM (test . entryKey . getEntry keyOp) items
        when b $ removeDirectoryRecursive dir
        return b
