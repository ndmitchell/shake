
module Main(main) where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Environment
import General.Timing
import Development.Shake.FileInfo
import General.String
import qualified Data.ByteString.Char8 as BS
import Test.Type(sleepFileTimeCalibrate)
import Control.Concurrent

import qualified Test.Assume as Assume
import qualified Test.Basic as Basic
import qualified Test.Benchmark as Benchmark
import qualified Test.C as C
import qualified Test.Cache as Cache
import qualified Test.Command as Command
import qualified Test.Config as Config
import qualified Test.Digest as Digest
import qualified Test.Directory as Directory
import qualified Test.Docs as Docs
import qualified Test.Errors as Errors
import qualified Test.Files as Files
import qualified Test.FilePath as FilePath
import qualified Test.FilePattern as FilePattern
import qualified Test.Journal as Journal
import qualified Test.Lint as Lint
import qualified Test.Live as Live
import qualified Test.Makefile as Makefile
import qualified Test.Manual as Manual
import qualified Test.Monad as Monad
import qualified Test.Ninja as Ninja
import qualified Test.Oracle as Oracle
import qualified Test.OrderOnly as OrderOnly
import qualified Test.Pool as Pool
import qualified Test.Progress as Progress
import qualified Test.Random as Random
import qualified Test.Resources as Resources
import qualified Test.Self as Self
import qualified Test.Tar as Tar
import qualified Test.Throttle as Throttle
import qualified Test.Unicode as Unicode
import qualified Test.Util as Util
import qualified Test.Verbosity as Verbosity

import qualified Start as Start


fakes = ["clean" * clean, "test" * test, "make" * makefile, "filetime" * filetime]
    where (*) = (,)

mains = ["tar" * Tar.main, "self" * Self.main, "c" * C.main
        ,"basic" * Basic.main, "cache" * Cache.main, "command" * Command.main
        ,"config" * Config.main, "digest" * Digest.main, "directory" * Directory.main
        ,"docs" * Docs.main, "errors" * Errors.main, "orderonly" * OrderOnly.main
        ,"filepath" * FilePath.main, "filepattern" * FilePattern.main, "files" * Files.main
        ,"journal" * Journal.main, "lint" * Lint.main, "live" * Live.main, "makefile" * Makefile.main, "manual" * Manual.main
        ,"monad" * Monad.main, "pool" * Pool.main, "random" * Random.main, "ninja" * Ninja.main
        ,"resources" * Resources.main, "assume" * Assume.main, "benchmark" * Benchmark.main
        ,"oracle" * Oracle.main, "progress" * Progress.main, "unicode" * Unicode.main, "util" * Util.main
        ,"throttle" * Throttle.main, "verbosity" * Verbosity.main]
    where (*) = (,)


main :: IO ()
main = do
    resetTimings
    xs <- getArgs
    exePath <- getExecutablePath
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
