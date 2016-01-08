{-# LANGUAGE CPP, ScopedTypeVariables #-}

module General.FileLock(withLockFile) where

import Control.Exception.Extra
import System.Directory
import System.FilePath
#ifdef mingw32_HOST_OS
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
#else
import System.IO
import System.Posix.IO
#endif

#ifdef mingw32_HOST_OS

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateFileW" c_CreateFileW :: Ptr CWchar -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Ptr () -> IO (Ptr ())
foreign import CALLCONV unsafe "Windows.h CloseHandle" c_CloseHandle :: Ptr () -> IO Bool
foreign import CALLCONV unsafe "Windows.h GetLastError" c_GetLastError :: IO Word32

c_GENERIC_WRITE = 0x40000000 :: Word32
c_GENERIC_READ  = 0x80000000 :: Word32
c_FILE_SHARE_NONE = 0 :: Word32
c_OPEN_ALWAYS = 4 :: Word32
c_FILE_ATTRIBUTE_NORMAL = 0x80 :: Word32
c_INVALID_HANDLE_VALUE = intPtrToPtr (-1)
c_ERROR_SHARING_VIOLATION = 32
#endif

withLockFile :: FilePath -> IO a -> IO a

#ifdef mingw32_HOST_OS

withLockFile file act = withCWString file $ \cfile -> do
    createDirectoryIfMissing True $ takeDirectory file
    let open = c_CreateFileW cfile (c_GENERIC_READ .|. c_GENERIC_WRITE) c_FILE_SHARE_NONE nullPtr c_OPEN_ALWAYS c_FILE_ATTRIBUTE_NORMAL nullPtr
    bracket open c_CloseHandle $ \h ->
        if h == c_INVALID_HANDLE_VALUE then do
            err <- c_GetLastError
            errorIO $ "Shake failed to acquire a file lock on " ++ file ++ "\n" ++
                      (if err == c_ERROR_SHARING_VIOLATION
                          then "ERROR_SHARING_VIOLATION - Shake is probably already running."
                          else "Code " ++ show err ++ ", unknown reason for failure.")
        else
            act

#else

withLockFile file act = do
    createDirectoryIfMissing True $ takeDirectory file
    try $ writeFile file "" :: IO (Either IOException ())

    bracket (openFd file ReadWrite Nothing defaultFileFlags) closeFd $ \fd -> do
        let lock = (WriteLock, AbsoluteSeek, 0, 0)
        res <- try $ setLock fd lock
        case res of
            Right () -> act
            Left (e :: IOException) -> do
                res <- getLock fd lock
                errorIO $ "Shake failed to acquire a file lock on " ++ file ++ "\n" ++
                          (case res of
                               Nothing -> ""
                               Just (pid, _) -> "Shake process ID " ++ show pid ++ " is using this lock.\n") ++
                          show e

#endif
