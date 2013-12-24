{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Development.Shake.FileTime(
    FileTime, fileTimeNone,
    getModTimeError, getModTimeMaybe
    ) where

import Development.Shake.Classes
import General.String
import Data.Char
import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as BS
import System.IO.Error
import Control.Exception
import Numeric

-- Required for Portable
import System.Directory
import Data.Time
import System.Time

-- Required for non-portable Windows
#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C.Types
type WIN32_FILE_ATTRIBUTE_DATA = Ptr ()
type LPCSTR = Ptr CChar
foreign import stdcall unsafe "Windows.h GetFileAttributesExA" c_getFileAttributesEx :: LPCSTR -> Int32 -> WIN32_FILE_ATTRIBUTE_DATA -> IO Bool
size_WIN32_FILE_ATTRIBUTE_DATA = 36
index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20
#endif

-- Required for non-portable Unix (since it requires a non-standard library, only require if not portable)
#if !defined(PORTABLE) && !defined(mingw32_HOST_OS)
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
getModTimeMaybe x = getModTimeMaybePortable x
#elif defined(mingw32_HOST_OS)
getModTimeMaybe x = getModTimeMaybeWindows x
#else
getModTimeMaybe x = getModTimeMaybeUnix x
#endif



-- Portable fallback
getModTimeMaybePortable :: BSU -> IO (Maybe FileTime)
getModTimeMaybePortable x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    time <- getModificationTime $ unpackU x
    return $ Just $ extractFileTime time

-- deal with difference in return type of getModificationTime between directory versions
class ExtractFileTime a where extractFileTime :: a -> FileTime
instance ExtractFileTime ClockTime where extractFileTime (TOD t _) = fileTime $ fromIntegral t
instance ExtractFileTime UTCTime where extractFileTime = fileTime . floor . fromRational . toRational . utctDayTime


-- Directly against the Win32 API, twice as fast as the portable version
#if defined(mingw32_HOST_OS)
getModTimeMaybeWindows :: BSU -> IO (Maybe FileTime)
getModTimeMaybeWindows x = BS.useAsCString (unpackU_ x) $ \file ->
    allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA $ \info -> do
        res <- c_getFileAttributesEx file 0 info
        if res then do
            -- Technically a Word32, but we can treak it as an Int32 for peek
            dword <- peekByteOff info index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime :: IO Int32
            return $ Just $ fileTime dword
         else if requireU x then
            getModTimeMaybePortable x
         else
            return Nothing
#endif


-- Unix version
#if !defined(PORTABLE) && !defined(mingw32_HOST_OS)
getModTimeMaybeUnix :: BSU -> IO (Maybe FileTime)
getModTimeMaybeUnix x = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing) (const $ return Nothing) $ do
    t <- fmap modificationTime $ getFileStatus $ unpackU_ x
    return $ Just $ fileTime $ fromIntegral $ fromEnum t
#endif
