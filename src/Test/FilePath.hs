
module Test.FilePath(main) where

import Development.Shake.FilePath
import qualified System.FilePath as Native
import qualified System.FilePath.Posix as Posix
import Test.Type
import Test.QuickCheck
import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Development.Shake.ByteString as BS
import System.Info.Extra


main = shaken test $ \args obj -> return ()


newtype File = File String deriving Show

instance Arbitrary File where
    arbitrary = fmap File $ listOf $ oneof $ map return "a /\\:."
    shrink (File x) = map File $ shrink x


test build obj = do
    let a === b = a Test.Type.=== b -- duplicate definition in QuickCheck 2.7 and above

    let norm x =
            let s = normalise x
                b = BS.unpack (BS.filepathNormalise $ BS.pack x)
            in if s == b then s else error $ show ("Normalise functions differ",x,s,b)
    -- basic examples
    norm "" === "."
    norm "." === "."
    norm "/" === "/"
    norm "./" === "./"
    norm "/." === "/."
    norm "/./" === "/"
    norm "a/." === "a"
    norm "./a" === "a"
    norm "./a/." === "a"
    norm "./a/./" === "a/"
    norm "a/.." === "."
    norm "a/./.." === "."
    norm "a/../" === "./"
    norm "/a/../" === "/"
    norm "/a/./../" === "/"
    norm "../a" === "../a"
    norm "/../a/" === "/../a/"

    -- more realistic examples
    norm "neil//./test/moo/../bar/bob/../foo" === "neil/test/bar/foo"
    norm "bar/foo" === "bar/foo"
    norm "bar/foo/" === "bar/foo/"
    norm "../../foo" === "../../foo"
    norm "foo/../..///" === "../"
    norm "foo/bar/../../neil" === "neil"
    norm "foo/../bar/../neil" === "neil"
    norm "/foo/bar" === "/foo/bar"
    norm "//./" === (if isWindows then "//" else "/")
    norm "//foo/./bar" === (if isWindows then "//foo/bar" else "/foo/bar")
    when isWindows $ norm "c:\\foo\\bar" === "c:/foo/bar"
    Success{} <- quickCheckWithResult stdArgs{maxSuccess=1000} $ \(File x) ->
        let y = norm x
            sep = Native.isPathSeparator
            noDrive = if isWindows then drop 1 else id
            ps = [length y >= 1
                 ,null x || (sep (head x) == sep (head y) && sep (last x) == sep (last y))
                 ,not $ "/./" `isInfixOf` y
                 ,not isWindows || '\\' `notElem` y
                 ,not $ "//" `isInfixOf` noDrive y
                 ,".." `notElem` dropWhile (== "..") (splitDirectories $ dropWhile sep y)
                 ,norm y == y]
        in if and ps then True else error $ show (x, y, ps)

    dropDirectory1 "aaa/bbb" === "bbb"
    dropDirectory1 "aaa/" === ""
    dropDirectory1 "aaa" === ""
    dropDirectory1 "" === ""

    takeDirectory1 "aaa/bbb" === "aaa"
    takeDirectory1 "aaa/" === "aaa"
    takeDirectory1 "aaa" === "aaa"

    searchPathSeparator === Native.searchPathSeparator
    pathSeparators === Posix.pathSeparators
