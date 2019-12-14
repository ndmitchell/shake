
module Test.Forward(main) where

import Control.Monad
import Data.Char
import Data.List.Extra
import Development.Shake
import Development.Shake.Forward
import Development.Shake.FilePath
import Test.Type
import System.IO.Extra as IO


main = testBuild test $ forwardRule $ do
    cs <- getDirectoryFiles "" ["*.c"]
    os <- forP cs $ \c -> do
        let o = c <.> "o"
        cache $ cmd "gcc -c" [c] "-o" [o]
        return o
    cache $ cmd "gcc -o" ["Main" <.> exe] os
    cache $ cmd ["Main" <.> exe] (FileStdout "output.txt")

    -- Doing this way to test cacheAction with arguments
    -- any real code should use a tracked readFile and avoid passing arguments to the closure
    src <- liftIO $ IO.readFile' "output.txt"
    cacheActionWith "reducer" src $ writeFile' "out.txt" $ filter isUpper src


test build = do
    b <- hasTracker
    unless b $
        putStrLn "Warning: Running forward test but without --forward mode (no tracker)"

    -- first clean then copy the source files over
    build ["clean"]
    copyDirectoryChanged (shakeRoot </> "src/Test/C") "."

    -- build and rebuild
    build ["--forward" | b]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HSU"

    -- check that cacheAction doesn't rerun when it shouldn't
    writeFile "out.txt" "HHH"
    build $ "-j2" : ["--forward" | b]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HHH"

    -- modify the constants
    orig <- IO.readFile' "constants.c"
    writeFile "constants.c" $ replace "Shake" "Rattle" orig
    build $ "-j2" : ["--forward" | b]
    assertContents "output.txt" "Hello Rattle Users!\n"
    assertContents "out.txt" "HRU"

    -- put it back
    writeFile "constants.c" orig
    build $ "-j2" : ["--forward" | b]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HSU"
