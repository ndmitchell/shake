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
import System.Time


newtype FileTime = FileTime Int
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)


getModTimeMaybe :: FilePath -> IO (Maybe FileTime)
getModTimeMaybe x = do
    b <- doesFileExist x
    if b then fmap Just $ getModTime x else return Nothing


getModTimeError :: String -> FilePath -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Important to raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n" ++ x
        Just x -> return x


getModTime :: FilePath -> IO FileTime
getModTime x = do
    TOD t _ <- getModificationTime x
    return $ FileTime $ fromIntegral t
