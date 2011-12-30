{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.FileTime(
    FileTime, getModTimeError, getModTimeMaybe
    ) where

import Control.Concurrent(threadDelay)
import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.List
import Data.Typeable
import System.Directory
import System.Time


newtype FileTime = FileTime Int
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)


getModTimeMaybe :: FilePath -> IO (Maybe FileTime)
getModTimeMaybe x = do
    b <- doesFileExist x
    if not b then return Nothing else do
        TOD t _ <- getModificationTime x
        return $ Just $ FileTime $ fromIntegral t


getModTimeError :: String -> FilePath -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Important to raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n" ++ x
        Just x -> return x
