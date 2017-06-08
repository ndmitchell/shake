
module Test.Live(main) where

import Development.Shake
import Test.Type


main = shakeTest_ test $ do
    let obj = id

    obj "foo" %> \ out -> do
        need [obj "bar"]
        writeFile' out ""

    obj "bar" %> \out -> writeFile' out ""
    obj "baz" %> \out -> writeFile' out ""


test build = do
    let obj = id
    build ["clean"]
    build ["foo","baz","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar baz"
    build ["foo","baz","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar baz"
    build ["foo","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "foo bar"
    build ["bar","--live=" ++ obj "live.txt"]
    assertContentsUnordered (obj "live.txt") $ map obj $ words "bar"
