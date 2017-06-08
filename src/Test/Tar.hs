
module Test.Tar(main) where

import Development.Shake
import System.FilePath
import Test.Type


main = shakeTest_ noTest $ do
    want ["result.tar"]
    "result.tar" %> \out -> do
        contents <- fmap (map (root </>)) $ readFileLines $ root </> "src/Test/Tar/list.txt"
        need contents
        cmd "tar -cf" [out] contents
