
module Examples.Test.Directory(main) where

import Development.Shake
import Examples.Util
import System.Directory(createDirectory)


main = shaken test $ \args obj -> do
    want $ map obj ["files.lst","dirs.lst","exist.lst","foodirexist.lst"]
    obj "files.lst" *> \out -> do
        x <- getDirectoryFiles (obj "") "*.txt"
        writeFileLines out x
    obj "dirs.lst" *> \out -> do
        x <- getDirectoryDirs (obj "")
        writeFileLines out x
    obj "exist.lst" *> \out -> do
        xs <- readFileLines $ obj "files.lst"
        ys <- mapM (doesFileExist . obj) $ xs ++ map reverse xs
        writeFileLines out $ map show ys
    obj "foodirexist.lst" *> \out -> do
        x <- doesDirectoryExist $ obj "Foo.txt"
        writeFileLines out [show x]

test build obj = do
    build ["clean"]
    build []
    assertContents (obj "files.lst") $ unlines []
    assertContents (obj "dirs.lst") $ unlines []
    assertContents (obj "exist.lst") $ unlines []
    assertContents (obj "foodirexist.lst") $ unlines ["False"]

    writeFile (obj "A.txt") ""
    writeFile (obj "B.txt") ""
    createDirectory (obj "Foo.txt")
    build ["--sleep"]
    assertContents (obj "files.lst") $ unlines ["A.txt","B.txt"]
    assertContents (obj "dirs.lst") $ unlines ["Foo.txt"]
    assertContents (obj "exist.lst") $ unlines ["True","True","False","False"]
    assertContents (obj "foodirexist.lst") $ unlines ["True"]
