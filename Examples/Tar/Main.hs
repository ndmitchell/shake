
module Examples.Tar.Main(main) where

import Development.Shake


main :: IO ()
main = shake shakeOptions{shakeFiles="output/tar", shakeVerbosity=2} $ do
    want ["output/tar/result.tar"]
    "output/tar/result.tar" *> \out -> do
        contents <- readFileLines "Examples/Tar/list.txt"
        need contents
        system' "tar" $ ["-cf",out] ++ contents
