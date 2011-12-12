
module Main where

import System.Directory
import System.Environment

import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self


main :: IO ()
main = do
    xs <- getArgs
    case xs of
        "clean":_ -> error "todo: clean"
        "tar":xs -> mkdir "output/tar" >> withArgs xs Tar.main
        "self":xs -> mkdir "output/self" >> withArgs xs Self.main
        _ -> error "Enter a command to continue"


mkdir = createDirectoryIfMissing True
