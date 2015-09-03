
module Test.FilePattern(main) where

import Development.Shake.FilePattern
import Development.Shake.FilePath
import Data.Tuple.Extra
import Control.Monad
import Data.List
import Test.Type
import Test.QuickCheck hiding ((===))

main = shaken test $ \args obj -> return ()


newtype Pattern = Pattern FilePattern deriving (Show,Eq)
newtype Path    = Path    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary Pattern where
    arbitrary = fmap Pattern $ listOf $ elements "\\/*ab"
    shrink (Pattern x) = map Pattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary Path where
    arbitrary = fmap Path $ listOf $ elements "\\/ab"
    shrink (Path x) = map Path $ shrinkList (\x -> ['/' | x == '\\']) x


test build obj = do
    let f b pat file = do
            assert (b == (pat `eval` file)) $ show pat ++ " `eval` " ++ show file ++ "\nEXPECTED: " ++ show b
            assert (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
            when b $ assert (toStandard (substitute (extract pat file) pat) == toStandard file) $ show pat ++ " " ++ show file ++ "\nFAILED substitute/extract property"

    f True "//*.c" "foo/bar/baz.c"
    f True (toNative "//*.c") "foo/bar\\baz.c"
    f True "*.c" "baz.c"
    f True "//*.c" "baz.c"
    f True "test.c" "test.c"
    f False "*.c" "foor/bar.c"
    f False "*/*.c" "foo/bar/baz.c"
    f False "foo//bar" "foobar"
    f False "foo//bar" "foobar/bar"
    f False "foo//bar" "foo/foobar"
    f True "foo//bar" "foo/bar"
    f True "foo/bar" (toNative "foo/bar")
    f True (toNative "foo/bar") "foo/bar"
    f True (toNative "foo/bar") (toNative "foo/bar")
    f True "//*" "/bar"
    f True "/bob//foo" "/bob/this/test/foo"
    f False "/bob//foo" "bob/this/test/foo"
    f True "bob//foo/" "bob/this/test/foo/"
    f False "bob//foo/" "bob/this/test/foo"
    f True "a//" "a"
    f True "/a//" "/a"
    f True "///a//" "/a"
    f False "///" ""
    f True "///" "/"
    f True "////" ""
    f True "x///y" "x/y"
    f True "x///" "x/"
    f True "x///" "x/foo/"
    f False "x///" "x"
    f True "x///" "x/foo/bar/"
    f False "x///" "x/foo/bar"
    f True "x///y" "x/z/y"

    simple "a*b" === False
    simple "a//b" === False
    simple "/a/b/cccc_" === True
    simple "a///b" === False

    assert (compatible []) "compatible"
    assert (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assert (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    extract (toNative "//*a*.txt") "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"

    directories1 "*.xml" === ("",False)
    directories1 "//*.xml" === ("",True)
    directories1 "foo//*.xml" === ("foo",True)
    first toStandard (directories1 "foo/bar/*.xml") === ("foo/bar",False)
    directories1 "*/bar/*.xml" === ("",True)
    directories ["*.xml","//*.c"] === [("",True)]
    directories ["bar/*.xml","baz//*.c"] === [("bar",False),("baz",True)]
    directories ["bar/*.xml","baz//*.c"] === [("bar",False),("baz",True)]

    Success{} <- quickCheckWithResult stdArgs{maxSuccess=1000} $ \(Pattern p) (Path x) ->
        if not $ eval p x then label "No match" $ not $ p ?== x
        else property $ p ?== x && toStandard (substitute (extract p x) p) == toStandard x
    return ()


eval :: FilePattern -> FilePath -> Bool
eval a b = f True (simp $ toStandard a) (toStandard b)
    where
        simp ('/':'/':'/':'/':xs) = simp ('/':'/':xs)
        simp ('*':'*':xs) = simp ('*':xs)
        simp (x:xs) = x : simp xs
        simp [] = []

        -- start = am I at the beginning of the pattern, not the matcher
        f start ('*':xs) (y:ys) = (y /= '/' && f False ('*':xs) ys) || f False xs (y:ys)
        f start ('*':xs) [] = f False xs []
        f start o@('/':'/':xs) ys
            | null xs && null ys = True -- at the end, it's all fine
            | '/':ys <- ys = ((start || "/" `isPrefixOf` xs) && f start xs ('/':ys)) || f False xs ys || f False o (dropWhile (/= '/') ys)
            | start = f start xs ys || f False o (dropWhile (/= '/') ys)
            | otherwise = False
        f start (x:xs) (y:ys) | x == y = f False xs ys
        f start [] [] = True
        f _ _ _ = False
