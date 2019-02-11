{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.History.Symlink(
    copyFileLink
    ) where

import System.Directory

{-
#ifndef mingw32_HOST_OS
import System.Posix.Files(createLink)
#else

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h CreateHardLinkW" c_CreateHardLinkW :: Ptr CWchar -> Ptr CWchar -> Ptr () -> IO Bool

createLink :: FilePath -> FilePath -> IO ()
createLink from to = withCWString from $ \cfrom -> withCWString to $ \cto -> do
    res <- c_CreateHardLinkW cfrom cto nullPtr
    unless res $ error $ show ("Failed to createLink", from, to)

#endif
-}


copyFileLink :: FilePath -> FilePath -> IO ()
copyFileLink from to = copyFile from to
