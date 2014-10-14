
module Test.Tar(main) where

import Development.Shake
import Test.Type


main = shaken noTest $ \args obj -> do
    want $ obj "result.tar"
    obj "result.tar" *> \out -> do
        contents <- readFileLines "Test/Tar/list.txt"
        need contents
        cmd "tar -cf" [out] contents
