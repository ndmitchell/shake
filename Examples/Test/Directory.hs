
module Examples.Test.Directory(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import System.Directory(createDirectory)
import Data.List


    -- Use escape characters, _o=* _l=/ __=<space>
readEsc ('_':'o':xs) = '*' : readEsc xs
readEsc ('_':'l':xs) = '/' : readEsc xs
readEsc ('_':'_':xs) = ' ' : readEsc xs
readEsc (x:xs) = x : readEsc xs
readEsc [] = []

showEsc = concatMap f
    where f '*' = "_o"
          f '/' = "_l"
          f ' ' = "__"
          f x = [x]


main = shaken test $ \args obj -> do
    want $ map obj args
    obj "*.contents" *> \out ->
        writeFileLines out =<< getDirectoryContents (obj $ readEsc $ dropExtension $ unobj out)
    obj "*.dirs" *> \out ->
        writeFileLines out =<< getDirectoryDirs (obj $ readEsc $ dropExtension $ unobj out)
    obj "*.files" *> \out -> do
        let pats = readEsc $ dropExtension $ unobj out
        let (x:xs) = ["" | " " `isPrefixOf` pats] ++ words pats
        writeFileLines out =<< getDirectoryFiles (obj x) xs

    obj "*.exist" *> \out -> do
        let xs = map obj $ words $ readEsc $ dropExtension $ unobj out
        fs <- mapM doesFileExist xs
        ds <- mapM doesDirectoryExist xs
        let bool x = if x then "1" else "0"
        writeFileLines out $ zipWith (\a b -> bool a ++ bool b) fs ds


test build obj = do
    let demand x ys = let f = showEsc x in do build [f]; assertContents (obj f) $ unlines $ words ys
    build ["clean"]
    demand " *.txt.files" ""
    demand ".dirs" ""
    demand "A.txt B.txt C.txt.exist" "00 00 00"

    writeFile (obj "A.txt") ""
    writeFile (obj "B.txt") ""
    createDirectory (obj "C.txt")
    demand " *.txt.files" "A.txt B.txt"
    demand ".dirs" "C.txt"
    demand "A.txt B.txt C.txt.exist" "10 10 01"
