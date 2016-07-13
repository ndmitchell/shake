
module Test.Version(main) where

import Development.Shake
import Test.Type


main = shakenCwd test $ \args obj -> do
    want [obj "foo.txt"]
    obj "foo.txt" %> \file -> liftIO $ appendFile file "x"

test build obj = do
    writeFile (obj "foo.txt") ""
    v1 <- getHashedShakeVersion [obj "foo.txt"]
    writeFile (obj "foo.txt") "y"
    v2 <- getHashedShakeVersion [obj "foo.txt"]
    assertBool (v1 /= v2) "Hashes must not be equal"

    build ["clean"]
    build []
    assertContents (obj "foo.txt") "x"
    build ["--rule-version=new","--silent"]
    assertContents (obj "foo.txt") "xx"
    build ["--rule-version=new"]
    assertContents (obj "foo.txt") "xx"
    build ["--rule-version=extra","--silent"]
    assertContents (obj "foo.txt") "xxx"
    build ["--rule-version=more","--no-rule-version"]
    assertContents (obj "foo.txt") "xxx"
    build ["--rule-version=more"]
    assertContents (obj "foo.txt") "xxx"
    build ["--rule-version=final","--silent"]
    assertContents (obj "foo.txt") "xxxx"
