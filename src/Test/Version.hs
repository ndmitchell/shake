
module Test.Version(main) where

import Development.Shake
import General.GetOpt
import Text.Read.Extra
import Test.Type


data Opts = Ver Int
opts = [Option "" ["ver"] (ReqArg (fmap Ver . readEither) "INT") ""]


main = shakeTest test opts $ \opts -> do
    want ["foo.txt"]
    "foo.txt" %> \file -> liftIO $ appendFile file "x"

    let ver = head $ [x | Ver x <- opts] ++ [0]
    versioned ver $ "ver.txt" %> \out -> liftIO $ appendFile out $ show ver
    "unver.txt" %> \out -> liftIO $ appendFile out "x"
    phony "version" $ need ["ver.txt","unver.txt"]


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

    build ["version"]
    assertContents "ver.txt" "0"
    assertContents "unver.txt" "x"
    build ["version","--ver=0"]
    assertContents "ver.txt" "0"
    build ["version","--ver=8"]
    build ["version","--ver=9"]
    build ["version","--ver=9"]
    assertContents "ver.txt" "089"
    assertContents "unver.txt" "x"
