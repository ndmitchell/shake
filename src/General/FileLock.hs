{-# LANGUAGE CPP #-}

module General.FileLock(withLockFile) where

#ifdef mingw32_HOST_OS
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Exception.Extra
#else
import qualified System.Posix.IO as IO
#endif

#ifdef mingw32_HOST_OS
foreign import stdcall unsafe "Windows.h CreateFileW" c_CreateFileW :: Ptr CWchar -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Ptr () -> IO (Ptr ())
foreign import stdcall unsafe "Windows.h CloseHandle" c_CloseHandle :: Ptr () -> IO Bool
foreign import stdcall unsafe "Windows.h GetLastError" c_GetLastError :: IO Word32

c_GENERIC_READ = 0x80000000 :: Word32
c_FILE_SHARE_NONE = 0 :: Word32
c_OPEN_ALWAYS = 4 :: Word32
c_FILE_ATTRIBUTE_NORMAL = 0x80 :: Word32
c_INVALID_HANDLE_VALUE = intPtrToPtr (-1)
c_ERROR_SHARING_VIOLATION = 32
#endif

withLockFile :: FilePath -> IO a -> IO a

#ifdef mingw32_HOST_OS

withLockFile file act = withCWString file $ \cfile -> do
    let open = c_CreateFileW cfile c_GENERIC_READ c_FILE_SHARE_NONE nullPtr c_OPEN_ALWAYS c_FILE_ATTRIBUTE_NORMAL nullPtr
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
    try $ writeFile file :: IO (Either IOException ())

    bracket (IO.openFd fn IO.ReadWrite Nothing IO.defaultFileFlags) IO.closeFd $ \fd -> do
        let lock = (IO.WriteLock, IO.AbsoluteSeek, 0, 0)
        res <- try $ IO.setLock fd lock
        case res of
            Right () -> act
            Left (e :: IOException) -> do
                res <- IO.getLock fd lock
                errorIO $ "Shake failed to acquire a file lock on " ++ file ++ "\n" ++
                          (case res of Nothing -> ""; Just (pid, _) ->
                              "Shake appears to be using this lock with process ID " ++ show pid ++ "\n") ++
                          show e

#endif
