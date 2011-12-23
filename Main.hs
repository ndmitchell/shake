
module Main where

import Data.Maybe
import System.Environment

import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.Test.Basic1 as Basic1
import qualified Examples.Test.Directory as Directory
import qualified Examples.Test.Errors as Errors
import qualified Examples.Test.Files as Files


fakes = ["clean" * clean, "test" * test]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main
        ,"basic1" * Basic1.main, "directory" * Directory.main, "errors" * Errors.main, "files" * Files.main]
    where (*) = (,)


main :: IO ()
main = do
    xs <- getArgs
    case flip lookup (fakes ++ mains) =<< listToMaybe xs of
        Nothing -> error $ "Enter one of the examples: " ++ unwords (map fst $ fakes ++ mains)
        Just main -> main


clean :: IO ()
clean = sequence_ [withArgs [name,"clean"] main | (name,main) <- mains]


test :: IO ()
test = sequence_ [withArgs [name,"test"] main | (name,main) <- mains]
