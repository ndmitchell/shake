-- | Fake cabal module for local building

module Paths_shake where

import Data.Version
import System.IO.Unsafe
import System.Directory
import Control.Exception
import Text.ParserCombinators.ReadP


-- We want getDataFileName to be relative to the current directory even if
-- we issue a change directory command. Therefore, first call caches, future ones read.
curdir :: String
curdir = unsafePerformIO getCurrentDirectory

getDataFileName :: FilePath -> IO FilePath
getDataFileName x = do
    evaluate curdir
    return $ curdir ++ "/" ++ x

version :: Version
-- can't write a literal Version value since in GHC 7.10 the versionsTag field is deprecated
version = head $ [v | (v,"") <- readP_to_S parseVersion "0.0"] ++ error "version, failed to parse"
