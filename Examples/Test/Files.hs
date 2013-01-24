
module Examples.Test.Files(main) where

import Development.Shake
import Examples.Util
import Control.Monad
import Data.List


main = shaken test $ \args obj -> do
    want $ map obj ["even.txt","odd.txt"]

    let deps = map obj ["even.txt","odd.txt"]
    let def | "fun" `elem` args = (?>>) (\x -> if x `elem` deps then Just deps else Nothing)
            | otherwise = (*>>) deps

    def $ \[evens,odds] -> do
        src <- readFileLines $ obj "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os


test build obj = do
    forM_ [[],["fun"]] $ \args -> do
        let nums = unlines . map show
        writeFile (obj "numbers.txt") $ nums [1,2,4,5,2,3,1]
        build ("--sleep":args)
        assertContents (obj "even.txt") $ nums [2,4,2]
        assertContents (obj "odd.txt" ) $ nums [1,5,3,1]
