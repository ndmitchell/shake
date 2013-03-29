
module Main(main) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import Development.Shake.Pool

import Examples.Util(sleepFileTime)
import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.C.Main as C
import qualified Examples.Test.Assume as Assume
import qualified Examples.Test.Basic as Basic
import qualified Examples.Test.Benchmark as Benchmark
import qualified Examples.Test.Cache as Cache
import qualified Examples.Test.Command as Command
import qualified Examples.Test.Directory as Directory
import qualified Examples.Test.Docs as Docs
import qualified Examples.Test.Errors as Errors
import qualified Examples.Test.Files as Files
import qualified Examples.Test.FilePath as FilePath
import qualified Examples.Test.FilePattern as FilePattern
import qualified Examples.Test.Journal as Journal
import qualified Examples.Test.Lint as Lint
import qualified Examples.Test.Makefile as Makefile
import qualified Examples.Test.Oracle as Oracle
import qualified Examples.Test.Pool as Pool
import qualified Examples.Test.Progress as Progress
import qualified Examples.Test.Random as Random
import qualified Examples.Test.Resources as Resources

import qualified Development.Make.Main as Make


fakes = ["clean" * clean, "test" * test, "make" * makefile]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main, "c" * C.main
        ,"basic" * Basic.main, "cache" * Cache.main, "command" * Command.main, "directory" * Directory.main
        ,"docs" * Docs.main, "errors" * Errors.main
        ,"filepath" * FilePath.main, "filepattern" * FilePattern.main, "files" * Files.main
        ,"journal" * Journal.main, "lint" * Lint.main, "makefile" * Makefile.main
        ,"pool" * Pool.main, "random" * Random.main
        ,"resources" * Resources.main, "assume" * Assume.main, "benchmark" * Benchmark.main
        ,"oracle" * Oracle.main, "progress" * Progress.main]
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
            ,""
            ,"As an example, try:"
            ,""
            ,"  main self --threads2 --loud"
            ,""
            ,"Which will build Shake, using Shake, on 2 threads."]
        Just main -> main sleepFileTime


makefile :: IO () -> IO ()
makefile _ = do
    args <- getArgs
    withArgs (drop 1 args) Make.main


clean :: IO () -> IO ()
clean extra = sequence_ [withArgs [name,"clean"] $ main extra | (name,main) <- mains]


test :: IO () -> IO ()
test _ = do
    args <- getArgs
    let tests = filter ((/= "random") . fst) mains
    let (priority,normal) = partition (flip elem ["assume","journal"] . fst) tests
    flip onException (putStrLn "TESTS FAILED") $
        execute sleepFileTime [\pause -> withArgs (name:"test":drop 1 args) $ test pause | (name,test) <- priority ++ normal]


-- | Execute each item in the list. They may yield (call the first parameter) in which case
--   you must execute yield for each one of them.
execute :: IO () -> [IO () -> IO ()] -> IO ()
execute yield acts = runPool True 1 $ \pool -> do
    let pause = blockPool pool $ yield >> return (False,())
    forM_ acts $ \act -> addPool pool $ act pause
