
module Examples.Util where

import Development.Shake
import Development.Shake.FilePath

import System.Directory
import System.Environment


shaken :: ((String -> String) -> Rules ()) -> IO ()
shaken rules = do
    name:args <- getArgs
    let out = "output/" ++ name ++ "/"
    createDirectoryIfMissing True out
    withArgs args $ shake shakeOptions{shakeFiles=out} $ rules (out++)


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1
