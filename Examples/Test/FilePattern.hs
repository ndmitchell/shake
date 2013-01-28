
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
    f False "foo//bar" "foobar"
    f False "foo//bar" "foobar/bar"
    f False "foo//bar" "foo/foobar"
    f True "foo//bar" "foo/bar"

    assert (compatible []) "compatible"
    assert (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assert (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"

    directories1 "*.xml" === ("",False)
    directories1 "//*.xml" === ("",True)
    directories1 "foo//*.xml" === ("foo",True)
    directories1 "foo/bar/*.xml" === ("foo/bar",False)
    directories1 "*/bar/*.xml" === ("",True)
    directories ["*.xml","//*.c"] === [("",True)]
    directories ["bar/*.xml","baz//*.c"] === [("bar",False),("baz",True)]
    directories ["bar/*.xml","baz//*.c"] === [("bar",False),("baz",True)]


---------------------------------------------------------------------
-- LAZY SMALLCHECK PROPERTIES

{-
newtype Pattern = Pattern FilePattern deriving (Show,Eq)
newtype Path    = Path    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Serial Pattern where
    series = cons Pattern >< f
        where f = cons [] \/ cons (:) >< const (drawnFrom "/*ab") >< f

instance Serial Path where
    series = cons Path >< f
        where f = cons [] \/ cons (:) >< const (drawnFrom "/ab") >< f

testSmallCheck = do
    smallCheck 10 $ \(Pattern p) (Path x) -> p ?== x ==> substitute (extract p x) p == x
-}
