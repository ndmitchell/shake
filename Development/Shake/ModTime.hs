{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.ModTime(
    ModTime, getModTimeError, getModTimeMaybe
    ) where

import Data.Binary
import Data.Hashable
import Data.List
import Data.Typeable
import System.Directory
import System.Time


newtype ModTime = ModTime Int
    deriving (Typeable,Eq,Hashable,Binary,Show)


getModTimeMaybe :: FilePath -> IO (Maybe ModTime)
getModTimeMaybe x = do
    b <- doesFileExist x
    if not b then return Nothing else do
        TOD t _ <- getModificationTime x
        return $ Just $ ModTime $ fromIntegral t


getModTimeError :: String -> FilePath -> IO ModTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Important to raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n" ++ x
        Just x -> return x
