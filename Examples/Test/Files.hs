
module Examples.Test.Files(main) where

import Development.Shake
import Examples.Util
import Data.List


main = shaken test $ \obj -> do
    want $ map obj ["even.txt","odd.txt"]
    map obj ["even.txt","odd.txt"] *>> \[evens,odds] -> do
        src <- readFileLines $ obj "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os


test build obj = do
    let nums = unlines . map show
    writeFile (obj "numbers.txt") $ nums [1,2,4,5,2,3,1]
    build []
    assertContents (obj "even.txt") $ nums [2,4,2]
    assertContents (obj "odd.txt" ) $ nums [1,5,3,1]
