
module Examples.Test.FilePath(main) where

import Development.Shake.FilePath
import Examples.Util


main = shaken test $ \args obj -> return ()


test build obj = do
    normalise "neil//./test/moo/../bar/bob/../foo" === "neil/test/bar/foo"
    normalise "bar/foo" === "bar/foo"
    normalise "bar/foo/" === "bar/foo/"
    normalise "../../foo" === "../../foo"
    normalise "foo/bar/../../neil" === "neil"
    normalise "foo/../bar/../neil" === "neil"
