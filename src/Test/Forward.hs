
module Test.Forward(main) where

import Development.Shake
import Development.Shake.Forward
import Development.Shake.FilePath
import Control.Monad.Extra
import Test.Type

main = shakeTest_ test $ forwardRule $ do
    let src = root </> "src/Test/C"
    cs <- getDirectoryFiles src ["*.c"]
    os <- forP cs $ \c -> do
        let o = c <.> "o"
        cache $ cmd "gcc -c" [src </> c] "-o" [o]
        return o
    cache $ cmd "gcc -o" ["Main" <.> exe] os

test build =
    whenM hasTracker $ do
        build ["--forward","--clean"]
        build ["--forward","-j2"]
