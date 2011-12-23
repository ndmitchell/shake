
module Examples.Test.Files(main) where

import Development.Shake
import Development.Shake.FilePattern
import Examples.Util
import Data.List


main = shaken test $ \args obj -> do
    want $ map obj ["even.txt","odd.txt"]
    map obj ["even.txt","odd.txt"] *>> \[evens,odds] -> do
        src <- readFileLines $ obj "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os


test build obj = do
    assert (compatible []) "compatible"
    assert (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assert (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"

    let nums = unlines . map show
    writeFile (obj "numbers.txt") $ nums [1,2,4,5,2,3,1]
    build []
    assertContents (obj "even.txt") $ nums [2,4,2]
    assertContents (obj "odd.txt" ) $ nums [1,5,3,1]
