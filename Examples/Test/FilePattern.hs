
module Examples.Test.FilePattern(main) where

import Development.Shake
import Examples.Util

main = shaken test $ \args obj -> return ()


test build obj = do
    let f b pat file = assert (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
    f True "//*.c" "foo/bar/baz.c"
    f True "*.c" "baz.c"
    f True "//*.c" "baz.c"
    f True "test.c" "test.c"
    f False "*.c" "foor/bar.c"
    f False "*/*.c" "foo/bar/baz.c"
