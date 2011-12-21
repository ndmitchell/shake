
module Examples.Util where

import Development.Shake
import Development.Shake.FilePath

import Control.Concurrent(threadDelay)
import Control.Monad
import System.Directory
import System.Environment


shaken
    :: (([String] -> IO ()) -> (String -> String) -> IO ())
    -> ((String -> String) -> Rules ())
    -> IO ()
shaken test rules = do
    name:args <- getArgs
    let out = "output/" ++ name ++ "/"
    createDirectoryIfMissing True out
    let run = shake shakeOptions{shakeFiles=out} $ rules (out++)
    case args of
        "test":_ -> do
            putStrLn $ "## TESTING " ++ name
            test (`withArgs` run) (out++)
        "clean":_ -> putStrLn $ "TODO: rm -rf " ++ out
        _ -> withArgs args run


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1


assertContents :: FilePath -> String -> IO ()
assertContents file want = do
    got <- readFile file
    when (want /= got) $ error $ "File contents are wrong: " ++ file ++ show (want,got)


noTest :: ([String] -> IO ()) -> (String -> String) -> IO ()
noTest build obj = do
    build []
    build []


sleep :: Double -> IO ()
sleep x = threadDelay $ floor $ x * 1000000
