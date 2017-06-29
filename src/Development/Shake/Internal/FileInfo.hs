{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Development.Shake.Internal.FileInfo(
    FileInfo, fileInfoNoHash,
    FileSize, ModTime, FileHash,
    getFileHash, getFileInfo
    ) where

import Control.Exception.Extra
import Development.Shake.Classes
import Development.Shake.Internal.Errors
import Development.Shake.Internal.FileName
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Word
import Numeric
import System.IO
import Foreign

#if defined(PORTABLE)
import System.IO.Error
import System.Directory
import Data.Time
#if __GLASGOW_HASKELL__ < 706
import System.Time
#endif

#elif defined(mingw32_HOST_OS)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Foreign.C.Types
import Foreign.C.String

#else
import GHC.IO.Exception
import System.IO.Error
import System.Posix.Files.ByteString
#endif

-- A piece of file information, where 0 and 1 are special (see fileInfo* functions)
newtype FileInfo a = FileInfo Word32
    deriving (Typeable,Hashable,Binary,Storable,NFData)

fileInfoNoHash :: FileInfo FileInfoHash
fileInfoNoHash = FileInfo 1   -- Equal to nothing

fileInfo :: Word32 -> FileInfo a
fileInfo a = FileInfo $ if a > maxBound - 2 then a else a + 2

instance Show (FileInfo a) where
    show (FileInfo x)
        | x == 0 = "EQ"
        | x == 1 = "NEQ"
        | otherwise = "0x" ++ map toUpper (showHex (x-2) "")

instance Eq (FileInfo a) where
    FileInfo a == FileInfo b
        | a == 0 || b == 0 = True
        | a == 1 || b == 1 = False
        | otherwise = a == b

data FileInfoHash; type FileHash = FileInfo FileInfoHash
data FileInfoMod ; type ModTime  = FileInfo FileInfoMod
data FileInfoSize; type FileSize = FileInfo FileInfoSize


getFileHash :: FileName -> IO FileHash
getFileHash x = withFile (fileNameToString x) ReadMode $ \h -> do
    s <- LBS.hGetContents h
    let res = fileInfo $ fromIntegral $ hash s
    evaluate res
    return res

-- If the result isn't strict then we are referencing a much bigger structure,
-- and it causes a space leak I don't really understand on Linux when running
-- the 'tar' test, followed by the 'benchmark' test.
-- See this blog post: http://neilmitchell.blogspot.co.uk/2015/09/three-space-leaks.html
result :: Word32 -> Word32 -> IO (Maybe (ModTime, FileSize))
result x y = do
    x <- evaluate $ fileInfo x
    y <- evaluate $ fileInfo y
    return $ Just (x, y)


getFileInfo :: FileName -> IO (Maybe (ModTime, FileSize))

#if defined(PORTABLE)
-- Portable fallback
getFileInfo x = handleBool isDoesNotExistError (const $ return Nothing) $ do
    let file = fileNameToString x
    time <- getModificationTime file
    size <- withFile file ReadMode hFileSize
    result (extractFileTime time) (fromIntegral size)

-- deal with difference in return type of getModificationTime between directory versions
class ExtractFileTime a where extractFileTime :: a -> Word32
#if __GLASGOW_HASKELL__ < 706
instance ExtractFileTime ClockTime where extractFileTime (TOD t _) = fromIntegral t
#endif
instance ExtractFileTime UTCTime where extractFileTime = floor . fromRational . toRational . utctDayTime


#elif defined(mingw32_HOST_OS)
-- Directly against the Win32 API, twice as fast as the portable version
getFileInfo x = BS.useAsCString (fileNameToByteString x) $ \file ->
    alloca_WIN32_FILE_ATTRIBUTE_DATA $ \fad -> do
        res <- c_GetFileAttributesExA file 0 fad
        code <- peekFileAttributes fad
        let peek = do
                code <- peekFileAttributes fad
                if testBit code 4 then
                    errorDirectoryNotFile $ fileNameToString x
                 else
                    join $ liftM2 result (peekLastWriteTimeLow fad) (peekFileSizeLow fad)
        if res then
            peek
         else if BS.any (>= chr 0x80) (fileNameToByteString x) then withCWString (fileNameToString x) $ \file -> do
            res <- c_GetFileAttributesExW file 0 fad
            if res then peek else return Nothing
         else
            return Nothing

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h GetFileAttributesExA" c_GetFileAttributesExA :: Ptr CChar  -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool
foreign import CALLCONV unsafe "Windows.h GetFileAttributesExW" c_GetFileAttributesExW :: Ptr CWchar -> Int32 -> Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Bool

data WIN32_FILE_ATTRIBUTE_DATA

alloca_WIN32_FILE_ATTRIBUTE_DATA :: (Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO a) -> IO a
alloca_WIN32_FILE_ATTRIBUTE_DATA act = allocaBytes size_WIN32_FILE_ATTRIBUTE_DATA act
    where size_WIN32_FILE_ATTRIBUTE_DATA = 36

peekFileAttributes :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekFileAttributes p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_dwFileAttributes
    where index_WIN32_FILE_ATTRIBUTE_DATA_dwFileAttributes = 0

peekLastWriteTimeLow :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekLastWriteTimeLow p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime
    where index_WIN32_FILE_ATTRIBUTE_DATA_ftLastWriteTime_dwLowDateTime = 20

peekFileSizeLow :: Ptr WIN32_FILE_ATTRIBUTE_DATA -> IO Word32
peekFileSizeLow p = peekByteOff p index_WIN32_FILE_ATTRIBUTE_DATA_nFileSizeLow
    where index_WIN32_FILE_ATTRIBUTE_DATA_nFileSizeLow = 32


#else
-- Unix version
getFileInfo x = handleBool isDoesNotExistError' (const $ return Nothing) $ do
    s <- getFileStatus $ fileNameToByteString x
    if isDirectory s then
        errorDirectoryNotFile $ fileNameToString x
     else
        result (extractFileTime s) (fromIntegral $ fileSize s)
    where
        isDoesNotExistError' e =
            isDoesNotExistError e || ioeGetErrorType e == InappropriateType

extractFileTime :: FileStatus -> Word32
#ifndef MIN_VERSION_unix
#define MIN_VERSION_unix(a,b,c) 0
#endif
#if MIN_VERSION_unix(2,6,0)
extractFileTime x = ceiling $ modificationTimeHiRes x * 1e4 -- precision of 0.1ms
#else
extractFileTime x = fromIntegral $ fromEnum $ modificationTime x
#endif

#endif
