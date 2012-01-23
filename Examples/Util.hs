
module Examples.Util(module Examples.Util, sleepFileTime) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.FileTime

import Control.Concurrent
import Control.Monad
import System.Directory as IO
import System.Environment


shaken
    :: (([String] -> IO ()) -> (String -> String) -> IO ())
    -> ([String] -> (String -> String) -> Rules ())
    -> IO ()
shaken test rules = do
    name:args <- getArgs
    putStrLn $ "## BUILD " ++ unwords (name:args)
    let out = "output/" ++ name ++ "/"
    createDirectoryIfMissing True out
    case args of
        "test":_ -> do
            putStrLn $ "## TESTING " ++ name
            test (\args -> withArgs (name:args) $ shaken test rules) (out++)
        "clean":_ -> removeDirectoryRecursive out
{-
        "lint":args -> do
            let dbfile = out ++ ".database"
                tempfile = "output/" ++ name ++ ".database"
            b <- IO.doesFileExist dbfile
            when b $ renameFile dbfile tempfile
            removeDirectoryRecursive out
            createDirectoryIfMissing True out
            when b $ renameFile tempfile dbfile
            shake shakeOptions{shakeFiles=out, shakeLint=True} $ rules args (out++)
-}
        _ -> shake shakeOptions{shakeFiles=out, shakeDump=True} $ rules args (out++)


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1

assert :: Bool -> String -> IO ()
assert b msg = unless b $ error $ "ASSERTION FAILED: " ++ msg


(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assert (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b


assertContents :: FilePath -> String -> IO ()
assertContents file want = do
    got <- readFile file
    assert (want == got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got


noTest :: ([String] -> IO ()) -> (String -> String) -> IO ()
noTest build obj = do
    build []
    build []


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
