{-# LANGUAGE CPP #-}

module Development.Shake.Internal.History.Symlink(
    copyFileLink
    ) where

import Control.Monad.Extra
import General.Extra
import System.Directory
import System.FilePath


#ifdef mingw32_HOST_OS
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
#else
import System.Posix.Files(createLink)
#endif

createLinkBool :: FilePath -> FilePath -> IO (Maybe String)

#ifdef mingw32_HOST_OS

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateHardLinkW " c_CreateHardLinkW :: Ptr CWchar -> Ptr CWchar -> Ptr () -> IO Bool

createLinkBool from to = withCWString from $ \cfrom -> withCWString to $ \cto -> do
    res <- c_CreateHardLinkW cto cfrom nullPtr
    return $ if res then Nothing else Just "CreateHardLink failed."

#else

createLinkBool from to = handleIO (return . Just . show) $ createLink from to >> return Nothing

#endif


copyFileLink :: FilePath -> FilePath -> IO ()
copyFileLink from to = do
    createDirectoryRecursive $ takeDirectory to
    removeFile_ to
    b <- createLinkBool from to
    whenJust b $ \_ ->
        copyFile from to
    -- making files read only stops them from easily deleting
    when False $
        forM_ [from, to] $ \x -> do
            perm <- getPermissions x
            setPermissions x perm{writable=False}
