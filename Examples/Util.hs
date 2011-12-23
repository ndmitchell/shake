
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
    case args of
        "test":_ -> do
            putStrLn $ "## TESTING " ++ name
            test (\args -> withArgs (name:args) $ shaken test rules) (out++)
        "clean":_ -> removeDirectoryRecursive out
        _ -> withArgs args $ shake shakeOptions{shakeFiles=out} $ rules (out++)


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
sleep x = threadDelay $ floor $ x * 1000000
