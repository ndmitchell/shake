module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment

import Examples.Util(sleepFileTime)
import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.C.Main as C
import qualified Examples.Test.Assume as Assume
import qualified Examples.Test.Basic as Basic
import qualified Examples.Test.Benchmark as Benchmark
import qualified Examples.Test.Directory as Directory
import qualified Examples.Test.Errors as Errors
import qualified Examples.Test.Files as Files
import qualified Examples.Test.FilePath as FilePath
import qualified Examples.Test.FilePattern as FilePattern
import qualified Examples.Test.Journal as Journal
import qualified Examples.Test.Oracle as Oracle
import qualified Examples.Test.Pool as Pool
import qualified Examples.Test.Progress as Progress
import qualified Examples.Test.Random as Random
import qualified Examples.Test.Resources as Resources


fakes = ["clean" * clean, "test" * test]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main, "c" * C.main
        ,"basic" * Basic.main, "directory" * Directory.main, "errors" * Errors.main
        ,"filepath" * FilePath.main, "filepattern" * FilePattern.main, "files" * Files.main
        ,"journal" * Journal.main, "pool" * Pool.main, "random" * Random.main
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


clean :: IO () -> IO ()
clean extra = sequence_ [withArgs [name,"clean"] $ main extra | (name,main) <- mains]


test :: IO () -> IO ()
test _ = do
    args <- getArgs
    one <- newMVar () -- Only one may execute at a time
    let pause = do putMVar one (); sleepFileTime; takeMVar one
    let tests = filter ((/= "random") . fst) mains
    -- priority tests have more pauses in, so doing them sooner gets the whole tests done faster
    self <- myThreadId
    let (priority,normal) = partition (flip elem ["assume","journal"] . fst) tests
    (dones,threads) <- fmap unzip $ forM (priority ++ normal) $ \(name,main) -> do
        done <- newEmptyMVar
        thread <- forkIO $ flip onException (killThread self) $ do
            takeMVar one
            withArgs (name:"test":drop 1 args) $ main pause
            putMVar one ()
            putMVar done ()
        return (done, thread)
    onException (mapM_ takeMVar dones) $ do
        mapM_ killThread threads
        putStrLn "TESTS FAILED"
