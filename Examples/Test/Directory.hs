
module Examples.Test.Directory(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import System.Directory(createDirectory)
import Data.List
import Control.Monad
import System.Directory(getCurrentDirectory, setCurrentDirectory)


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

    obj "dots" *> \out -> do
        cwd <- liftIO getCurrentDirectory
        liftIO $ setCurrentDirectory $ obj ""
        b1 <- liftM2 (==) (getDirectoryContents ".") (getDirectoryContents "")
        b2 <- liftM2 (==) (getDirectoryDirs ".") (getDirectoryDirs "")
        b3 <- liftM2 (==) (getDirectoryFiles "." ["*.txt"]) (getDirectoryFiles "" ["*.txt"])
        b4 <- liftM2 (==) (getDirectoryFiles "." ["C.txt/*.txt"]) (getDirectoryFiles "" ["C.txt/*.txt"])
        b5 <- liftM2 (==) (getDirectoryFiles "." ["//*.txt"]) (getDirectoryFiles "" ["//*.txt"])
        liftIO $ setCurrentDirectory cwd
        writeFileLines out $ map show [b1,b2,b3,b4,b5]

test build obj = do
    let demand x ys = let f = showEsc x in do build [f]; assertContents (obj f) $ unlines $ words ys
    build ["clean"]
    demand " *.txt.files" ""
    demand " //*.txt.files" ""
    demand ".dirs" ""
    demand "A.txt B.txt C.txt.exist" "00 00 00"

    writeFile (obj "A.txt") ""
    writeFile (obj "B.txt") ""
    createDirectory (obj "C.txt")
    writeFile (obj "C.txt/D.txt") ""
    writeFile (obj "C.txt/E.xtx") ""
    demand " *.txt.files" "A.txt B.txt"
    demand ".dirs" "C.txt"
    demand "A.txt B.txt C.txt.exist" "10 10 01"
    demand " //*.txt.files" "A.txt B.txt C.txt/D.txt"
    demand "C.txt *.txt.files" "D.txt"
    demand " *.txt //*.xtx.files" "A.txt B.txt C.txt/E.xtx"
    demand " C.txt/*.files" "C.txt/D.txt C.txt/E.xtx"

    build ["dots","--no-lint"]
    assertContents (obj "dots") $ unlines $ words "True True True True True"
