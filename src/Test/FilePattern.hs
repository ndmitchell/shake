
module Test.FilePattern(main) where

import Development.Shake.FilePattern
import Development.Shake.FilePath
import Control.Monad
import System.IO.Unsafe
import Data.List.Extra
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
    internalTest
    let f b pat file = do
            assert (b == (pat `eval` file)) $ show pat ++ " `eval` " ++ show file ++ "\nEXPECTED: " ++ show b
            assert (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
            assert (b == (pat `walker` file)) $ show pat ++ " `walker` " ++ show file ++ "\nEXPECTED: " ++ show b
            when b $ assert (toStandard (substitute (extract pat file) pat) == toStandard file) $
                "FAILED substitute/extract property\nPattern: " ++ show pat ++ "\nFile: " ++ show file ++ "\n" ++
                "Extracted: " ++ show (extract pat file) ++ "\nSubstitute: " ++ show (substitute (extract pat file) pat)

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
    f True "///" "a/"
    f True "////" ""
    f True "x///y" "x/y"
    f True "x///" "x/"
    f True "x///" "x/foo/"
    f False "x///" "x"
    f True "x///" "x/foo/bar/"
    f False "x///" "x/foo/bar"
    f True "x///y" "x/z/y"
    f True "" ""
    f False "" "y"
    f False "" "/"

    f True "*/*" "x/y"
    f False "*/*" "x"
    f True "//*" "x"
    f True "//*" ""
    f True "*//" "x"
    f True "*//" ""
    f True "*//*" "x/y"
    f False "*//*" ""
    f False "*//*" "x"
    f False "*//*//*" "x/y"
    f True "//*/" "/"
    f True "*/////" "/"
    f False "b*b*b*//" "bb"

    simple "a*b" === False
    simple "a//b" === False
    simple "/a/b/cccc_" === True
    simple "a///b" === False

    assert (compatible []) "compatible"
    assert (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assert (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "//a.txt" "a.txt" === [""]
    extract "//a.txt" "/a.txt" === ["/"]
    extract "a//b" "a/b" === [""]
    extract "a//b" "a/x/b" === ["x/"]
    extract "a//b" "a/x/y/b" === ["x/y/"]
    extract "a///b" "a/x/y/b" === ["x/y/"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    extract (toNative "//*a*.txt") "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"

    (False, Walk _) <- return $ walk ["*.xml"]
    (False, Walk _) <- return $ walk ["//*.xml"]
    (False, WalkTo ([], [("foo",Walk _)])) <- return $ walk ["foo//*.xml"]
    (False, WalkTo ([], [("foo",WalkTo ([],[("bar",Walk _)]))])) <- return $ walk ["foo/bar/*.xml"]
    (False, WalkTo (["a"],[("b",WalkTo (["c"],[]))])) <- return $ walk ["a","b/c"]
    ([], [("foo",WalkTo ([],[("bar",Walk _)]))]) <- let (False, Walk f) = walk ["*/bar/*.xml"] in return $ f ["foo"]
    (False, WalkTo ([],[("bar",Walk _),("baz",Walk _)])) <- return $ walk ["bar/*.xml","baz//*.c"]
    (False, WalkTo ([], [])) <- return $ walk []
    (True, Walk _) <- return $ walk ["//"]
    (True, WalkTo _) <- return $ walk [""]

    Success{} <- quickCheckWithResult stdArgs{maxSuccess=1000} $ \(Pattern p) (Path x) ->
        let b = eval p x in (if b then property else label "No match") $ unsafePerformIO $ do f b p x; return True
    return ()


walker :: FilePattern -> FilePath -> Bool
walker a b = f (split isPathSeparator b) $ snd $ walk [a]
    where
        f (x:xs) (Walk op) = f (x:xs) $ WalkTo $ op [x]
        f [x]    (WalkTo (file, dir)) = x `elem` file
        f (x:xs) (WalkTo (file, dir)) | Just w <- lookup x dir = f xs w
        f _ _ = False


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
