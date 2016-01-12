
module Test.Live(main) where

import Development.Shake
import Test.Type


main = shakenCwd test $ \args obj -> do
    want $ map obj args

    obj "foo" %> \ out -> do
        need [obj "bar"]
        writeFile' out ""

    obj "bar" %> \out -> writeFile' out ""
    obj "baz" %> \out -> writeFile' out ""


test build obj = do
    build ["clean"]
    build ["foo","baz","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar baz"
    build ["foo","baz","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar baz"
    build ["foo","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar"
    build ["bar","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "bar"
