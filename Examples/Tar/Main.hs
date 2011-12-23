
module Examples.Tar.Main(main) where

import Development.Shake
import Examples.Util


main :: IO ()
main = shaken noTest $ \args obj -> do
    want [obj "result.tar"]
    obj "result.tar" *> \out -> do
        contents <- readFileLines "Examples/Tar/list.txt"
        need contents
        system' "tar" $ ["-cf",out] ++ contents
