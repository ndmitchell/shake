
module Test.Files(main) where

import Development.Shake
import Test.Type
import Control.Monad
import Data.List


main = shaken test $ \args obj -> do
    let fun = "@" `elem` args
    let rest = delete "@" args
    want $ map obj $ if null rest then ["even.txt","odd.txt"] else rest

    -- Since &?> and &*> are implemented separately we test everything in both modes
    let deps &?*> act | fun = (\x -> if x `elem` deps then Just deps else Nothing) &?> act
                      | otherwise = deps &*> act

    map obj ["even.txt","odd.txt"] &?*> \[evens,odds] -> do
        src <- readFileLines $ obj "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os

    map obj ["dir1/out.txt","dir2/out.txt"] &?*> \[a,b] -> do
        writeFile' a "a"
        writeFile' b "b"


test build obj = do
    forM_ [[],["@"]] $ \args -> do
        let nums = unlines . map show
        writeFile (obj "numbers.txt") $ nums [1,2,4,5,2,3,1]
        build ("--sleep":args)
        assertContents (obj "even.txt") $ nums [2,4,2]
        assertContents (obj "odd.txt" ) $ nums [1,5,3,1]
        build ["clean"]
        build ["--no-build","--report=-"]
        build ["dir1/out.txt"]
