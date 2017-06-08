
module Test.Verbosity(main) where

import Development.Shake
import Test.Type


main = shakeTest_ test $ do
    "in.txt" %> \out -> do
        a <- getVerbosity
        b <- withVerbosity Normal getVerbosity
        writeFile' out $ unwords $ map show [a,b]

    "out.txt" %> \out -> do
        x <- getVerbosity
        ys <- withVerbosity Loud $ do
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
    assertContents "in.txt" "Normal Normal"
    assertContents "out.txt" "Normal Loud Loud Quiet Normal Normal"

    build ["out.txt","--clean","--verbose"]
    assertContents "in.txt" "Loud Normal"
    assertContents "out.txt" "Loud Loud Loud Quiet Loud Loud"

    build ["out.txt","--clean","--quiet"]
    assertContents "in.txt" "Quiet Normal"
    assertContents "out.txt" "Quiet Loud Loud Quiet Quiet Quiet"
