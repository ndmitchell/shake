
module Test.Verbosity(main) where

import Development.Shake
import Test.Type


main = shaken test $ \args obj -> do
    want $ map obj args

    obj "in.txt" %> \out -> do
        a <- getVerbosity
        b <- withVerbosity Normal getVerbosity
        writeFile' out $ unwords $ map show [a,b]

    obj "out.txt" %> \out -> do
        x <- getVerbosity
        ys <- withVerbosity Loud $ do
            a <- getVerbosity
            need [obj "in.txt"] -- make sure the inherited verbosity does not get passed along
            b <- getVerbosity
            c <- quietly getVerbosity
            d <- fmap shakeVerbosity getShakeOptions
            return [a,b,c,d]
        z <- getVerbosity
        writeFile' out $ unwords $ map show $ [x] ++ ys ++ [z]

test build obj = do
    build ["out.txt","--clean"]
    assertContents (obj "in.txt") "Normal Normal"
    assertContents (obj "out.txt") "Normal Loud Loud Quiet Normal Normal"

    build ["out.txt","--clean","--verbose"]
    assertContents (obj "in.txt") "Loud Normal"
    assertContents (obj "out.txt") "Loud Loud Loud Quiet Loud Loud"

    build ["out.txt","--clean","--quiet"]
    assertContents (obj "in.txt") "Quiet Normal"
    assertContents (obj "out.txt") "Quiet Loud Loud Quiet Quiet Quiet"
