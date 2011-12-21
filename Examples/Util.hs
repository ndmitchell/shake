
module Examples.Util where

import Development.Shake
import Development.Shake.FilePath


shaken :: String -> ((String -> String) -> Rules ()) -> IO ()
shaken name rules = shake shakeOptions{shakeFiles=out} $ rules (out++)
    where out = "output/" ++ name ++ "/"


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1
