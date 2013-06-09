{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Development.Shake.FileTime(
    FileTime, fileTimeNone,
    getModTimeError, getModTimeMaybe
    ) where

import Development.Shake.Classes
import Data.Int
import qualified Data.ByteString.Char8 as BS

#ifdef PORTABLE

import System.IO.Error
import Control.Exception
import System.Directory

#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif

#elif defined mingw32_HOST_OS

import Foreign
import Foreign.C.Types

type WIN32_FILE_ATTRIBUTE_DATA = Ptr ()
type LPCSTR = Ptr CChar

foreign import stdcall unsafe "Windows.h GetFileAttributesExA" c_getFileAttributesEx :: LPCSTR -> Int32 -> WIN32_FILE_ATTRIBUTE_DATA -> IO Bool

size_WIN32_FILE_ATTRIBUTE_DATA = 36

index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20

#else

import System.IO.Error
import Control.Exception
import System.Posix.Files.ByteString

#endif


-- FileTime is an optimised type, which stores some portion of the file time,
-- or maxBound to indicate there is no valid time. The moral type is @Maybe Datetime@
-- but it needs to be more efficient.
newtype FileTime = FileTime Int32
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)


fileTime :: Int32 -> FileTime
fileTime x = FileTime $ if x == maxBound then maxBound - 1 else x

fileTimeNone :: FileTime
fileTimeNone = FileTime maxBound


getModTimeMaybe :: BS.ByteString -> IO (Maybe FileTime)

#ifdef PORTABLE

-- Portable fallback
getModTimeMaybe x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    time <- getModificationTime $ BS.unpack x
#if __GLASGOW_HASKELL__ >= 706
    return $ Just $ fileTime $ floor $ utctDayTime time
#else
    let TOD t _ = time
    return $ Just $ fileTime $ fromIntegral t
#endif

#elif defined mingw32_HOST_OS

-- Directly against the Win32 API, twice as fast as the portable version
getModTimeMaybe x = BS.useAsCString x $ \file ->
    allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA $ \info -> do
        res <- c_getFileAttributesEx file 0 info
        if not res then return Nothing else do
            -- Technically a Word32, but we can treak it as an Int32 for peek
            dword <- peekByteOff info index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
            return $ Just $ fileTime dword

#else

-- Directly against the unix library
getModTimeMaybe x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    t <- fmap modificationTime $ getFileStatus x
    return $ Just $ fileTime $ fromIntegral $ fromEnum t

#endif


getModTimeError :: String -> BS.ByteString -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Make sure you raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n  " ++ BS.unpack x
        Just x -> return x
