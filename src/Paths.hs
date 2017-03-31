-- | Fake cabal module for local building

module Paths_shake(getDataFileName, version) where

import Data.Version.Extra
import System.IO.Unsafe
import System.Directory
import Control.Exception


-- We want getDataFileName to be relative to the current directory even if
-- we issue a change directory command. Therefore, first call caches, future ones read.
{-# NOINLINE curdir #-}
curdir :: String
curdir = unsafePerformIO getCurrentDirectory

getDataFileName :: FilePath -> IO FilePath
getDataFileName x = do
    evaluate curdir
    return $ curdir ++ "/" ++ x

version :: Version
version = makeVersion [0,0]
