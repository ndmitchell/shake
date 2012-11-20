module Main where

import Data.Maybe
import System.Environment

import Examples.Util(flags)
import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.C.Main as C
import qualified Examples.Test.Basic as Basic
import qualified Examples.Test.Directory as Directory
import qualified Examples.Test.Errors as Errors
import qualified Examples.Test.Files as Files
import qualified Examples.Test.FilePath as FilePath
import qualified Examples.Test.FilePattern as FilePattern
import qualified Examples.Test.Journal as Journal
import qualified Examples.Test.Pool as Pool
import qualified Examples.Test.Random as Random
import qualified Examples.Test.Resources as Resources


fakes = ["clean" * clean, "test" * test]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main, "c" * C.main
        ,"basic" * Basic.main, "directory" * Directory.main, "errors" * Errors.main
        ,"filepath" * FilePath.main, "filepattern" * FilePattern.main, "files" * Files.main
        ,"journal" * Journal.main, "pool" * Pool.main, "random" * Random.main
        ,"resources" * Resources.main]
    where (*) = (,)


main :: IO ()
main = do
    xs <- getArgs
    case flip lookup (fakes ++ mains) =<< listToMaybe xs of
        Nothing -> putStrLn $ unlines
            ["Welcome to the Shake demo"
            ,""
            ,unwords $ "Modes:" : map fst fakes
            ,unwords $ "Demos:" : map fst mains
            ,unwords $ "Flags:" : flags
            ,""
            ,"As an example, try:"
            ,""
            ,"  main self --threads2 --loud"
            ,""
            ,"Which will build Shake, using Shake, on 2 threads."]
        Just main -> main


clean :: IO ()
clean = sequence_ [withArgs [name,"clean"] main | (name,main) <- mains]


test :: IO ()
test = sequence_ [withArgs [name,"test"] main | (name,main) <- mains, name /= "random"]
