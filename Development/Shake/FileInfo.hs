{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Development.Shake.FileInfo(
    FileTime, fileTimeNone,
    getModTimeError, getModTimeMaybe
    ) where

import Development.Shake.Classes
import General.String
import Data.Char
import Data.Int
import Data.Word
import Numeric

#if defined(PORTABLE)
import System.IO.Error
import Control.Exception
import System.Directory
import Data.Time
import System.Time

#elif defined(mingw32_HOST_OS)
import qualified Data.ByteString.Char8 as BS
import Foreign
import Foreign.C.Types
import Foreign.C.String

#else
import System.IO.Error
import Control.Exception
import System.Posix.Files.ByteString
#endif


-- FileTime is an optimised type, which stores some portion of the file time,
-- or maxBound to indicate there is no valid time. The moral type is @Maybe Datetime@
-- but it needs to be more efficient.
newtype FileTime = FileTime Int32
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show FileTime where
    show (FileTime x) = "0x" ++ replicate (length s - 8) '0' ++ map toUpper s
        where s = showHex (fromIntegral x :: Word32) ""

fileTime :: Int32 -> FileTime
fileTime x = FileTime $ if x == maxBound then maxBound - 1 else x

fileTimeNone :: FileTime
fileTimeNone = FileTime maxBound


getModTimeError :: String -> BSU -> IO FileTime
getModTimeError msg x = do
    res <- getModTimeMaybe x
    case res of
        -- Make sure you raise an error in IO, not return a value which will error later
        Nothing -> error $ msg ++ "\n  " ++ unpackU x
        Just x -> return x


getModTimeMaybe :: BSU -> IO (Maybe FileTime)

#if defined(PORTABLE)
-- Portable fallback
getModTimeMaybe x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    time <- getModificationTime $ unpackU x
    return $ Just $ extractFileTime time

-- deal with difference in return type of getModificationTime between directory versions
class ExtractFileTime a where extractFileTime :: a -> FileTime
instance ExtractFileTime ClockTime where extractFileTime (TOD t _) = fileTime $ fromIntegral t
instance ExtractFileTime UTCTime where extractFileTime = fileTime . floor . fromRational . toRational . utctDayTime


#elif defined(mingw32_HOST_OS)
-- Directly against the Win32 API, twice as fast as the portable version
getModTimeMaybe x = BS.useAsCString (unpackU_ x) $ \file ->
    alloca_WIN32_FILE_ATTRIBUTE_DATA $ \fad -> do
        res <- c_getFileAttributesExA file 0 fad
        if res then
            fmap (Just . fileTime) $ peekLastWriteTimeLow fad
         else if requireU x then withCWString (unpackU x) $ \file -> do
            res <- c_getFileAttributesExW file 0 fad
            if res then fmap (Just . fileTime) $ peekLastWriteTimeLow fad else return Nothing
         else
            return Nothing

foreign import stdcall unsafe "Windows.h GetFileAttributesExA" c_getFileAttributesExA :: Ptr CChar  -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool
foreign import stdcall unsafe "Windows.h GetFileAttributesExW" c_getFileAttributesExW :: Ptr CWchar -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool

data WIN32_FILE_ATTRIBUTE_DATA

alloca_WIN32_FILE_ATTRIBUTE_DATA :: (Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO a) -> IO a
alloca_WIN32_FILE_ATTRIBUTE_DATA act = allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA act
    where size_WIN32_FILE_ATTRIBUTE_DATA = 36

peekLastWriteTimeLow :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Int32
peekLastWriteTimeLow p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
    where index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20


#else
-- Unix version
getModTimeMaybe x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    s <- getFileStatus $ unpackU_ x
    return $ Just $ fileTime $ extractFileTime s

extractFileTime :: FileStatus -> Int32
#ifndef MIN_VERSION_unix
#define MIN_VERSION_unix(a,b,c) 0
#endif
#if MIN_VERSION_unix(2,6,0)
extractFileTime x = ceiling $ modificationTimeHiRes x * 1e4 -- precision of 0.1ms
#else
extractFileTime x = fromIntegral $ fromEnum $ modificationTime x
#endif

#endif
