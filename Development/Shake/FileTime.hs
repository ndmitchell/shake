{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

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
import System.Time


newtype FileTime = FileTime Int
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)


getModTimeMaybe :: FilePath -> IO (Maybe FileTime)
getModTimeMaybe x =
    fmap Just (getModTime x) `catchIOError` \e ->
        if isDoesNotExistError e then return Nothing else ioError e


getModTimeError :: String -> FilePath -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Make sure you raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n" ++ x
        Just x -> return x


getModTime :: FilePath -> IO FileTime
getModTime x = do
    TOD t _ <- getModificationTime x
    return $ FileTime $ fromIntegral t