
module Test.Directory(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Data.List
import Control.Monad
import System.Directory(getCurrentDirectory, setCurrentDirectory, createDirectory, createDirectoryIfMissing)
import qualified System.Directory as IO


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
    obj "*.contents" %> \out ->
        writeFileLines out =<< getDirectoryContents (obj $ readEsc $ dropExtension $ unobj out)
    obj "*.dirs" %> \out ->
        writeFileLines out =<< getDirectoryDirs (obj $ readEsc $ dropExtension $ unobj out)
    obj "*.files" %> \out -> do
        let pats = readEsc $ dropExtension $ unobj out
        let (x:xs) = ["" | " " `isPrefixOf` pats] ++ words pats
        writeFileLines out =<< getDirectoryFiles (obj x) xs

    obj "*.exist" %> \out -> do
        let xs = map obj $ words $ readEsc $ dropExtension $ unobj out
        fs <- mapM doesFileExist xs
        ds <- mapM doesDirectoryExist xs
        let bool x = if x then "1" else "0"
        writeFileLines out $ zipWith (\a b -> bool a ++ bool b) fs ds

    obj "dots" %> \out -> do
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

    let removeTest pat del keep = do
            withTemporaryDirectory $ \dir -> do
                forM_ (del ++ keep) $ \s -> do
                    createDirectoryIfMissing True $ dir </> takeDirectory s
                    when (not $ hasTrailingPathSeparator s) $
                        writeFile (dir </> s) ""
                removeFiles dir pat
                createDirectoryIfMissing True dir
                forM_ (map ((,) False) del ++ map ((,) True) keep) $ \(b,s) -> do
                    b2 <- (if hasTrailingPathSeparator s then IO.doesDirectoryExist else IO.doesFileExist) $ dir </> s
                    when (b /= b2) $ do
                        let f b = if b then "present" else "missing"
                        error $ "removeFiles mismatch: with pattern " ++ show pat ++ ", " ++ s ++
                                " should be " ++ f b ++ " but is " ++ f b2

    removeTest ["//bob"] ["test/bob","more/bob"] ["extra/obo"]
    removeTest ["bob"] ["bob/"] ["bar/"]
    removeTest ["*.hs"] ["test.hs"] ["extra/more.hs","new.txt"]
    removeTest ["baz"] ["baz"] ["foo","bar/bob"]
    removeTest ["baz"] ["baz/bob","baz/"] ["foo","bar/bob"]
    removeTest ["Foo//*"] ["Foo/bar","Foo/Quux/bar","Foo/Quux/"] []
    removeTest ["Foo//*"] ["Foo/"] ["bar"]
    removeTest ["baz"] [] ["test.hs","bar/","foo/"]
    removeTest ["bob//*"] [] ["test/bob/"]
    removeTest ["//bob"] ["test/bob/","test/"] []
    removeTest ["//*.txt"] ["more/","more/a.txt"] []
    removeTest ["//*.txt"] ["more/","more/a.txt/"] []
    removeTest ["//*.txt"] ["more/","more/a.txt/","more/b.txt"] []
    removeTest ["//*.txt"] [] ["more/"]
    removeTest ["a//b"] ["a/c/b"] []
    removeFiles "non-existing-directory" ["*"]
