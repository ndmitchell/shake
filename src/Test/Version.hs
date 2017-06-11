
module Test.Version(main) where

import Development.Shake
import Test.Type


main = shakeTest_ test $ do
    want ["foo.txt"]
    "foo.txt" %> \file -> liftIO $ appendFile file "x"

test build = do
    writeFile "foo.txt" ""
    v1 <- getHashedShakeVersion ["foo.txt"]
    writeFile "foo.txt" "y"
    v2 <- getHashedShakeVersion ["foo.txt"]
    assertBool (v1 /= v2) "Hashes must not be equal"

    build ["clean"]
    build []
    assertContents "foo.txt" "x"
    build ["--rule-version=new","--silent"]
    assertContents "foo.txt" "xx"
    build ["--rule-version=new"]
    assertContents "foo.txt" "xx"
    build ["--rule-version=extra","--silent"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=more","--no-rule-version"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=more"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=final","--silent"]
    assertContents "foo.txt" "xxxx"
