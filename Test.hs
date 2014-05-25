{-# LANGUAGE CPP #-}

module Main(main) where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Environment
import General.Timing
import Development.Shake.FileInfo
import General.String
import qualified Data.ByteString.Char8 as BS
import Examples.Util(sleepFileTimeCalibrate)
import Control.Concurrent

import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.C.Main as C
import qualified Examples.Ninja.Main as Ninja
import qualified Examples.Test.Assume as Assume
import qualified Examples.Test.Basic as Basic
import qualified Examples.Test.Benchmark as Benchmark
import qualified Examples.Test.Cache as Cache
import qualified Examples.Test.Command as Command
import qualified Examples.Test.Config as Config
import qualified Examples.Test.Digest as Digest
import qualified Examples.Test.Directory as Directory
import qualified Examples.Test.Docs as Docs
import qualified Examples.Test.Errors as Errors
import qualified Examples.Test.Files as Files
import qualified Examples.Test.FilePath as FilePath
import qualified Examples.Test.FilePattern as FilePattern
import qualified Examples.Test.Journal as Journal
import qualified Examples.Test.Lint as Lint
import qualified Examples.Test.Makefile as Makefile
import qualified Examples.Test.Manual as Manual
import qualified Examples.Test.Oracle as Oracle
import qualified Examples.Test.OrderOnly as OrderOnly
import qualified Examples.Test.Pool as Pool
import qualified Examples.Test.Progress as Progress
import qualified Examples.Test.Random as Random
import qualified Examples.Test.Resources as Resources
import qualified Examples.Test.Throttle as Throttle
import qualified Examples.Test.Unicode as Unicode
import qualified Examples.Test.Util as Util
import qualified Examples.Test.Verbosity as Verbosity

import qualified Start as Start


fakes = ["clean" * clean, "test" * test, "make" * makefile, "filetime" * filetime]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main, "c" * C.main
        ,"basic" * Basic.main, "cache" * Cache.main, "command" * Command.main
        ,"config" * Config.main, "digest" * Digest.main, "directory" * Directory.main
        ,"docs" * Docs.main, "errors" * Errors.main, "orderonly" * OrderOnly.main
        ,"filepath" * FilePath.main, "filepattern" * FilePattern.main, "files" * Files.main
        ,"journal" * Journal.main, "lint" * Lint.main, "makefile" * Makefile.main, "manual" * Manual.main
        ,"pool" * Pool.main, "random" * Random.main, "ninja" * Ninja.main
        ,"resources" * Resources.main, "assume" * Assume.main, "benchmark" * Benchmark.main
        ,"oracle" * Oracle.main, "progress" * Progress.main, "unicode" * Unicode.main, "util" * Util.main
        ,"throttle" * Throttle.main, "verbosity" * Verbosity.main]
    where (*) = (,)


main :: IO ()
main = do
    resetTimings
    xs <- getArgs
#if __GLASGOW_HASKELL__ >= 706
    exePath <- getExecutablePath
#else
    exePath <- getProgName
#endif
    case flip lookup (fakes ++ mains) =<< listToMaybe xs of
        _ | null xs -> do
            putStrLn "******************************************************************"
            putStrLn "** Running shake test suite, run with '--help' to see arguments **"
            putStrLn "******************************************************************"
            withArgs ["test"] main
            withArgs ["random","test","3m"] main
        Nothing -> putStrLn $ unlines
            ["Welcome to the Shake demo"
            ,""
            ,unwords $ "Modes:" : map fst fakes
            ,unwords $ "Demos:" : map fst mains
            ,""
            ,"As an example, try:"
            ,""
            ,unwords ["  ", exePath, "self",  "--jobs=2", "--trace"]
            ,""
            ,"Which will build Shake, using Shake, on 2 threads."]
        Just main -> main =<< sleepFileTimeCalibrate


makefile :: IO () -> IO ()
makefile _ = do
    args <- getArgs
    withArgs (drop 1 args) Start.main


filetime :: IO () -> IO ()
filetime _ = do
    args <- getArgs
    addTiming "Reading files"
    files <- fmap concat $ forM (drop 1 args) $ \file ->
        fmap (BS.lines . BS.filter (/= '\r')) $ BS.readFile file
    let n = length files
    evaluate n
    addTiming "Modtime"
    let (a,bcd) = splitAt (n `div` 4) files
    let (b,cd) = splitAt (n `div` 4) bcd
    let (c,d) = splitAt (n `div` 4) cd
    vars <- forM [a,b,c,d] $ \xs -> do
        mvar <- newEmptyMVar
        forkIO $ do
            mapM_ (getFileInfo . packU_) xs
            putMVar mvar ()
        return $ takeMVar mvar
    sequence_ vars
    printTimings


clean :: IO () -> IO ()
clean extra = sequence_ [withArgs [name,"clean"] $ main extra | (name,main) <- mains]


test :: IO () -> IO ()
test yield = do
    args <- getArgs
    flip onException (putStrLn "TESTS FAILED") $
        sequence_ [withArgs (name:"test":drop 1 args) $ test yield | (name,test) <- mains, name /= "random"]
