
module Test.Forward(main) where

import Development.Shake
import Development.Shake.Forward
import Development.Shake.FilePath
import Test.Type

main = testBuild test $ forwardRule $ do
    let src = shakeRoot </> "src/Test/C"
    cs <- getDirectoryFiles src ["*.c"]
    os <- forP cs $ \c -> do
        let o = c <.> "o"
        cache $ cmd "gcc -c" [src </> c] "-o" [o]
        return o
    cache $ cmd "gcc -o" ["Main" <.> exe] os

test build = do
    b <- hasTracker
    build $ "--clean" : ["--forward" | b]
    build $ "-j2" : ["--forward" | b]
