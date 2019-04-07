{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Rattle.Hash(
    Hash, fromHash,
    hashFile, hashString
    ) where

import System.IO
import Data.Hashable
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import Data.Time
import System.Directory
import Data.Char
import System.IO.Unsafe
import System.IO.Error
import Control.Monad.Extra
import Data.IORef
import Numeric
import Control.Exception.Extra
import Control.DeepSeq


newtype Hash = Hash String
    deriving (NFData, Show, Read, Eq, Hashable)

fromHash :: Hash -> String
fromHash (Hash x) = x


mkHash :: BS.ByteString -> Hash
mkHash = Hash . concatMap (f . ord) . BS.unpack
    where f i = ['0' | i < 16] ++ showHex i ""


-- Hashing lots of files is expensive, so we keep a cache
{-# NOINLINE hashCache #-}
hashCache :: IORef (Map.HashMap FilePath (UTCTime, Hash))
hashCache = unsafePerformIO $ newIORef Map.empty


getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime x = handleBool isDoesNotExistError (const $ return Nothing) (Just <$> getModificationTime x)


hashFile :: FilePath -> IO (Maybe Hash)
hashFile file = do
    start <- getModTime file
    case start of
        Nothing -> return Nothing
        Just start -> do
            mp <- readIORef hashCache
            case Map.lookup file mp of
                Just (time, hash) | time == start -> return $ Just hash
                _ -> do
                    res <- withFile file ReadMode $ \h -> do
                        chunks <- LBS.hGetContents h
                        evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
                    end <- getModTime file
                    when (Just start == end) $
                        atomicModifyIORef' hashCache $ \mp -> (Map.insert file (start, res) mp, ())
                    return $ Just res


hashString :: String -> Hash
-- we first 'show' the String to avoid having > 256 characters in it
hashString = mkHash . SHA.hash . BS.pack . show
