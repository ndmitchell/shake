
module Test.Forward(main) where

import Development.Shake
import Development.Shake.Forward
import Development.Shake.FilePath
import Control.Monad.Extra
import Test.Type

main = shaken test $ \args obj -> forwardRule $ do
    let src = "src/Test/C"
    cs <- getDirectoryFiles src ["*.c"]
    os <- forP cs $ \c -> do
        let o = obj c <.> "o"
        cache $ cmd "gcc -c" [src </> c] "-o" [o]
        return o
    cache $ cmd "gcc -o" [obj "Main" <.> exe] os

test build obj = do
    whenM hasTracker $
        build ["--forward"]
