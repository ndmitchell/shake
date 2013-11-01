
module Examples.Test.FilePath(main) where

import Development.Shake.FilePath
import Development.Shake.Prelude
import Examples.Util


main = shaken test $ \args obj -> return ()


test build obj = do
    normalise "neil//./test/moo/../bar/bob/../foo" === "neil/test/bar/foo"
    normalise "bar/foo" === "bar/foo"
    normalise "bar/foo/" === "bar/foo/"
    normalise "../../foo" === "../../foo"
    normalise "foo/bar/../../neil" === "neil"
    normalise "foo/../bar/../neil" === "neil"
    normalise "//foo/./bar" === (if isWindows then "//foo/bar" else "/foo/bar")
    normalise "c:\\foo\\bar" === (if isWindows then "C:/foo/bar" else "c:\\foo\\bar")

    dropDirectory1 "aaa/bbb" === "bbb"
    dropDirectory1 "aaa/" === ""
    dropDirectory1 "aaa" === ""
    dropDirectory1 "" === ""

    takeDirectory1 "aaa/bbb" === "aaa"
    takeDirectory1 "aaa/" === "aaa"
    takeDirectory1 "aaa" === "aaa"

    combine "aaa/bbb" "ccc" === "aaa/bbb/ccc"
    combine "aaa/bbb" "./ccc" === "aaa/bbb/ccc"
    combine "aaa/bbb" "../ccc" === "aaa/ccc"
