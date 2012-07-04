{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.FileTime(
    FileTime,
    getModTimeError, getModTimeMaybe
    ) where

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Typeable
import System.Directory
import System.IO.Error
import Control.Exception
import System.Time
import qualified Data.ByteString.Char8 as BS


newtype FileTime = FileTime Int
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)


getModTimeMaybe :: BS.ByteString -> IO (Maybe FileTime)
getModTimeMaybe x =
    fmap Just (getModTime x) `Control.Exception.catch` \e ->
        if isDoesNotExistError e then return Nothing else ioError e


getModTimeError :: String -> BS.ByteString -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Make sure you raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n" ++ BS.unpack x
        Just x -> return x


getModTime :: BS.ByteString -> IO FileTime
getModTime x = do
    TOD t _ <- getModificationTime $ BS.unpack x
    return $ FileTime $ fromIntegral t
