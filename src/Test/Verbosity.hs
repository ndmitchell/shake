
module Test.Verbosity(main) where

import Development.Shake
import Test.Type


main = testBuild test $ do
    "in.txt" %> \out -> do
        a <- getVerbosity
        b <- withVerbosity Info getVerbosity
        writeFile' out $ unwords $ map show [a,b]

    "out.txt" %> \out -> do
        x <- getVerbosity
        ys <- withVerbosity Debug $ do
            a <- getVerbosity
            need ["in.txt"] -- make sure the inherited verbosity does not get passed along
            b <- getVerbosity
            c <- quietly getVerbosity
            d <- fmap shakeVerbosity getShakeOptions
            return [a,b,c,d]
        z <- getVerbosity
        writeFile' out $ unwords $ map show $ [x] ++ ys ++ [z]

test build = do
    build ["out.txt","--clean"]
    assertContents "in.txt" "Info Info"
    assertContents "out.txt" "Info Debug Debug Error Info Info"

    build ["out.txt","--clean","--verbose"]
    assertContents "in.txt" "Debug Info"
    assertContents "out.txt" "Debug Debug Debug Error Debug Debug"

    build ["out.txt","--clean","--quiet"]
    assertContents "in.txt" "Warn Info"
    assertContents "out.txt" "Warn Debug Debug Error Warn Warn"
