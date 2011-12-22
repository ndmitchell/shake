
module Examples.Test.Directory(main) where

import Development.Shake
import Examples.Util

main = shaken test $ \obj -> do
    want [obj "files.lst","dirs.lst","exist.lst"]
    obj "files.lst" *> \out ->
        x <- getDirectoryFiles "*.txt"
        writeFileLines out x
    obj "dirs.lst" *> \out ->
        x <- getDirectoryDirs
        writeFileLines out ""
    obj "exist.lst" *> \out ->
        xs <- readFileLines $ obj "files.lst"
        ys <- mapM doesFileExist $ xs ++ reverse xs
        writeFileLines out $ map show ys


test build obj = do
    build ["clean"]
    build []
    assertContents (obj "files.lst") $ unlines []
    assertContents (obj "dirs.lst") $ unlines []
    assertContents (obj "exist.lst") $ unlines []

    writeFile (obj "A.txt") ""
    writeFile (obj "B.txt") ""
    createDirectory "Foo.txt"
    build []
    assertContents (obj "files.lst") $ unlines ["A.txt","B.txt"]
    assertContents (obj "dirs.lst") $ unlines ["Foo.txt"]
    assertContents (obj "exist.lst") $ unlines ["True","True","False","False"]
