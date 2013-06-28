
module Examples.Tar.Main(main) where

import Development.Shake
import Examples.Util


main = shaken noTest $ \args obj -> do
    want [obj "result.tar"]
    obj "result.tar" *> \out -> do
        contents <- readFileLines "Examples/Tar/list.txt"
        need contents
        cmd "tar -cf" [out] contents
