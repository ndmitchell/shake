
module Examples.Test.FilePattern(main) where

import Development.Shake.FilePattern
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

    assert (compatible []) "compatible"
    assert (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assert (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"
