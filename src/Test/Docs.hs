{-# LANGUAGE PatternGuards #-}

module Test.Docs(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Control.Monad
import Data.Char
import Data.List.Extra
import Data.Maybe
import System.Info
import Data.Version.Extra


-- Older versions of Haddock garbage the --@ markup
brokenHaddock = compilerVersion < makeVersion [7,8]

main = shaken (\a b -> unless brokenHaddock $ noTest a b) $ \args obj -> do
    let index = obj "dist/doc/html/shake/index.html"
    let config = obj "dist/setup-config"
    want [obj "Success.txt"]

    want $ map (\x -> fromMaybe (obj x) $ stripPrefix "!" x) args

    let needSource = need =<< getDirectoryFiles "." ["src/Development/Shake.hs","src/Development/Shake//*.hs","src/Development/Ninja/*.hs","src/General//*.hs"]

    config %> \_ -> do
        need ["shake.cabal","Setup.hs"]
        -- Make Cabal and Stack play nicely
        path <- getEnv "GHC_PACKAGE_PATH"
        unit $ cmd (RemEnv "GHC_PACKAGE_PATH") "runhaskell Setup.hs configure"
            ["--builddir=" ++ obj "dist","--user"]
            -- package-db is very sensitive, see #267
            ["--package-db=" ++ x | x <- maybe [] (filter (`notElem` [".",""]) . splitSearchPath) path]
        trackAllow [obj "dist//*"]

    index %> \_ -> do
        need [config,"shake.cabal","Setup.hs","README.md","CHANGES.txt"]
        needSource
        trackAllow [obj "dist//*"]
        cmd "runhaskell Setup.hs haddock" ["--builddir=" ++ obj "dist"]

    obj "Paths_shake.hs" %> \out ->
        copyFile' "src/Paths.hs" out

    obj "Part_*.hs" %> \out -> do
        need ["src/Test/Docs.hs"] -- so much of the generator is in this module
        let noR = filter (/= '\r')
        src <- if "_md" `isSuffixOf` takeBaseName out then
            fmap (findCodeMarkdown . lines . noR) $ readFile' $ "docs/" ++ drop 5 (reverse (drop 3 $ reverse $ takeBaseName out)) ++ ".md"
         else
            fmap (findCodeHaddock . noR) $ readFile' $ obj $ "dist/doc/html/shake/" ++ replace "_" "-" (drop 5 $ takeBaseName out) ++ ".html"
        let f i (Stmt x) | any whitelist x = []
                         | otherwise = restmt i $ map undefDots $ trims x
            f i (Expr x) | takeWhile (not . isSpace) x `elem` types = ["type Expr_" ++ show i ++ " = " ++ x]
                         | "import " `isPrefixOf` x = [x]
                         | otherwise = ["expr_" ++ show i ++ " = (" ++ undefDots x2 ++ ")" | let x2 = trim $ dropComment x, not $ whitelist x2]
            code = concat $ zipWith f [1..] (nubOrd src)
            (imports,rest) = partition ("import " `isPrefixOf`) code
        writeFileLines out $
            ["{-# LANGUAGE DeriveDataTypeable, RankNTypes, MultiParamTypeClasses, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}"
            ,"{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}"
            ,"{-# OPTIONS_GHC -w #-}"
            ,"module " ++ takeBaseName out ++ "() where"
            ,"import Control.Applicative"
            ,"import Control.Concurrent"
            ,"import Control.Monad"
            ,"import Data.Char"
            ,"import Data.Data"
            ,"import Data.List"
            ,"import Data.Maybe"
            ,"import Data.Monoid"
            ,"import Development.Shake"
            ,"import Development.Shake.Classes"
            ,"import Development.Shake.Rule hiding (trackAllow)"
            ,"import Development.Shake.Util"
            ,"import Development.Shake.FilePath"
            ,"import System.Console.GetOpt"
            ,"import System.Directory(setCurrentDirectory)"
            ,"import qualified System.Directory"
            ,"import System.Process"
            ,"import System.Exit"
            ,"import Control.Monad.IO.Class"
            ,"import System.IO"] ++
            ["import " ++ replace "_" "." (drop 5 $ takeBaseName out) | not $ "_md.hs" `isSuffixOf` out] ++
            imports ++
            ["(==>) :: Bool -> Bool -> Bool"
            ,"(==>) = undefined"
            ,"(<==) = ()"
            ,"infix 1 ==>"
            ,"forAll f = f undefined"
            ,"remaining = 1.1"
            ,"done = 1.1"
            ,"time_elapsed = 1.1"
            ,"old = \"\""
            ,"new = \"\""
            ,"myfile = \"\""
            ,"inputs = [\"\"]"
            ,"files = [\"\"]"
            ,"input = \"\""
            ,"output = \"\""
            ,"opts = shakeOptions"
            ,"result = undefined :: IO (Maybe (Rules ()))"
            ,"launchMissiles = undefined :: Bool -> IO ()"
            ,"myVariable = ()"
            ,"instance Eq (OptDescr a)"
            ,"(foo,bar,baz) = undefined"
            ,"out = \"\""
            ,"str1 = \"\""
            ,"str2 = \"\""
            ,"str = \"\""] ++
            rest

    obj "Files.lst" %> \out -> do
        need ["src/Test/Docs.hs"] -- so much of the generator is in this module
        need [index,obj "Paths_shake.hs"]
        filesHs <- getDirectoryFiles (obj "dist/doc/html/shake") ["Development-*.html"]
        filesMd <- getDirectoryFiles "docs" ["*.md"]
        writeFileChanged out $ unlines $
            ["Part_" ++ replace "-" "_" (takeBaseName x) | x <- filesHs, not $ "-Classes.html" `isSuffixOf` x] ++
            ["Part_" ++ takeBaseName x ++ "_md" | x <- filesMd, takeBaseName x `notElem` ["Developing","Model"]]

    let needModules = do mods <- readFileLines $ obj "Files.lst"; need [obj m <.> "hs" | m <- mods]; return mods

    obj "Main.hs" %> \out -> do
        mods <- needModules
        writeFileLines out $ ["module Main(main) where"] ++ ["import " ++ m | m <- mods] ++ ["main = return ()"]

    obj "Success.txt" %> \out -> do
        needModules
        need [obj "Main.hs", obj "Paths_shake.hs"]
        needSource
        unit $ cmd "runhaskell -ignore-package=hashmap " ["-i" ++ obj "","-isrc",obj "Main.hs"]
        writeFile' out ""


---------------------------------------------------------------------
-- FIND THE CODE, MANIPULATE AS CODE

data Code = Stmt [String] | Expr String deriving (Show,Eq,Ord)


findCodeHaddock :: String -> [Code]
findCodeHaddock xs = f Expr "code" ++ f (Stmt . unindent . lines) "pre"
    where
        f op tag = map (op . innerText . checkItalic) $ insideTag tag xs

        checkItalic x | bad@(_:_) <- nubOrd (insideTag "em" x) \\ italics = error $ "Bad italics, " ++ show bad
                      | otherwise = x


findCodeMarkdown :: [String] -> [Code]
findCodeMarkdown (x:xs) | indented x && not (isBlank x) =
    let (a,b) = span (\x -> indented x || isBlank x) (x:xs)
    in Stmt (unindent a) : findCodeMarkdown b
    where
        indented x = length (takeWhile isSpace x) >= 4
findCodeMarkdown (x:xs) = map Expr (evens $ splitOn "`" x) ++ findCodeMarkdown xs
    where
        evens (x:y:xs) = y : evens xs
        evens _ = []
findCodeMarkdown [] = []


restmt :: Int -> [String] -> [String]
restmt i ("":xs) = restmt i xs
restmt i (('-':'-':_):xs) = restmt i xs
restmt i (x:xs) | " ?== " `isInfixOf` x || " == " `isInfixOf` x =
    zipWith (\j x -> "hack_" ++ show i ++ "_" ++ show j ++ " = " ++ x) [1..] (x:xs)
restmt i (x:xs) |
    not ("let" `isPrefixOf` x) && not ("[" `isPrefixOf` x) && not ("cmd " `isPrefixOf` x) && (" = " `isInfixOf` x || " | " `isInfixOf` x || " :: " `isInfixOf` x) ||
    "import " `isPrefixOf` x || "infix" `isPrefixOf` x || "instance " `isPrefixOf` x = map f $ x:xs
    where f x = if takeWhile (not . isSpace) x `elem` dupes then "_" ++ show i ++ "_" ++ x else x
restmt i xs = ("stmt_" ++ show i ++ " = do") : map ("  " ++) xs ++
              ["  undefined" | length xs == 1 && ("let" `isPrefixOf` head xs || "<-" `isInfixOf` head xs)]


---------------------------------------------------------------------
-- TEXT MANIPULATION

-- | Is a string empty or whitespace
isBlank :: String -> Bool
isBlank = all isSpace

-- | Remove leading and trailing blank lines (trim lifted to work on lines)
trims :: [String] -> [String]
trims = dropWhileEnd isBlank . dropWhile isBlank

-- | If all lines are indented by at least n spaces, then trim n spaces from each line
unindent :: [String] -> [String]
unindent xs = map (drop n) xs
    where n = minimum $ 1000 : map (length . takeWhile (== ' ')) (filter (not . isBlank) xs)

-- | Remove line comments from the end of lines
dropComment :: String -> String
dropComment = fst . breakOn "--"

-- | Replace ... with undefined (don't use undefined with cmd; two ...'s should become one replacement)
undefDots :: String -> String
undefDots x | Just x <- stripSuffix "..." x, Just (x,_) <- stripInfix "..." x = x ++ new
            | otherwise = replace "..." new x
    where new = if words x `disjoint` ["cmd","Development.Shake.cmd"] then "undefined" else "[\"\"]"

-- | Find all pieces of text inside a given tag
insideTag :: String -> String -> [String]
insideTag tag = map (fst . breakOn ("</" ++ tag ++ ">")) . drop 1 . splitOn ("<" ++ tag ++ ">")

-- | Given some HTML, find the raw text
innerText :: String -> String
innerText ('<':xs) = innerText $ drop 1 $ dropWhile (/= '>') xs
innerText ('&':xs)
    | Just xs <- stripPrefix "quot;" xs = '\"' : innerText xs
    | Just xs <- stripPrefix "lt;" xs = '<' : innerText xs
    | Just xs <- stripPrefix "gt;" xs = '>' : innerText xs
    | Just xs <- stripPrefix "amp;" xs = '&' : innerText xs
innerText (x:xs) = x : innerText xs
innerText [] = []


---------------------------------------------------------------------
-- DATA SECTION

-- | Only the following identifiers can appear in italic code blocks in Haddock
--   (otherwise it's a common markup mistake)
italics :: [String]
italics = words "command-name file-name N"

-- | Identifiers that indicate the fragment is a type
types :: [String]
types = words $
    "MVar IO String FilePath Maybe [String] Char ExitCode Change " ++
    "Action Resource Assume FilePattern Development.Shake.FilePattern " ++
    "Lint Verbosity Rules CmdOption Int Double"

-- | Duplicated identifiers which require renaming
dupes :: [String]
dupes = words "main progressSimple rules"


-- | Should a fragment be whitelisted and not checked
whitelist :: String -> Bool
whitelist x | all (not . isSpace) x && takeExtension x `elem` exts = True
    where exts = words ".txt .hi .hs .o .exe .tar .cpp .cfg .dep .deps .h .c .html .zip"
whitelist x | elem x $ words $
    "newtype do MyFile.txt.digits excel a q m c x value key gcc cl os make contents tar ghc cabal clean _make distcc " ++
    ".. /./ /.. /../ ./ // \\ ../ //*.c //*.txt //* dir/*/* dir " ++
    "ConstraintKinds TemplateHaskell GeneralizedNewtypeDeriving DeriveDataTypeable SetConsoleTitle " ++
    "Data.List System.Directory Development.Shake.FilePath main.m run .rot13 " ++
    "NoProgress Error src rot13 .js .json .trace about://tracing .ninja " ++
    ".make/i586-linux-gcc/output _make/.database foo/.. file.src file.out build " ++
    "/usr/special /usr/special/userbinary $CFLAGS %PATH% -O2 -j8 -j -j1 " ++
    "-threaded -rtsopts -I0 Hidden TypeRep extension $OUT $C_LINK_FLAGS $PATH xterm $TERM main opts result flagValues argValues " ++
    "HEADERS_DIR /path/to/dir CFLAGS let -showincludes -MMD gcc.version linkFlags temp pwd touch code out err " ++
    "_metadata/.shake.database _shake _shake/build ./build.sh build.sh build.bat [out] manual " ++
    "docs/manual _build _build/run ninja depfile build.ninja ByteString ProcessHandle " ++
    "Rule CmdResult ShakeValue Monoid Monad Eq Typeable Data " ++ -- work only with constraint kinds
    "@ndm_haskell file-name .PHONY filepath fsatrace base stack trim extra #include -j4 " ++
    "*> "
    = True
whitelist x | "Stdout out" `isInfixOf` x || "Stderr err" `isInfixOf` x = True
whitelist x
    | "foo/" `isPrefixOf` x -- path examples
    = True
whitelist x = x `elem`
    ["[Foo.hi, Foo.o]"
    ,"shake-progress"
    ,"main -j6"
    ,"main clean"
    ,"1m25s (15%)"
    ,"3m12s (82%)"
    ,"getPkgVersion $ GhcPkgVersion \"shake\""
    ,"# command-name (for file-name)"
    ,"ghc --make MyBuildSystem -rtsopts -with-rtsopts=-I0"
    ,"-with-rtsopts"
    ,"-qg -qb"
    ,"gcc -MM"
    ,"gcc -M"
    ,"shake -j"
    ,"# This is my Config file"
    ,"-g -I/path/to/dir -O2"
    ,"main _make/henry.txt"
    ,"<i>build rules</i>"
    ,"<i>actions</i>"
    ,"() <- cmd ..."
    ,"x <- inputs"
    ,"shakeFiles=\"_build\""
    ,"#include \""
    ,"pattern %> actions = (pattern ?==) ?> actions" -- because it overlaps
    ,"buildDir = \"_build\""
    ,"-MMD -MF"
    ,"#!/bin/sh"
    ,"build _build/main.o"
    ,"build clean"
    ,"build -j8"
    ,"cabal update && cabal install shake"
    ,"shake-build-system"
    ,"runhaskell Build.hs"
    ,"runhaskell Build.hs clean"
    ,"gcc -c main.c -o main.o -MMD -MF main.m"
    ,"\"_build\" </> x -<.> \"o\""
    ,"cmd \"gcc -o\" [out] os"
    ,"rot13 file.txt -o file.rot13"
    ,"file.rot13"
    ,"out -<.> \"txt\""
    ,"[item1,item2,item2]"
    ,"runhaskell Build.hs"
    ,"cabal update"
    ,"cabal install shake"
    ,"shake -j4"
    ,"cmd \"gcc -o _make/run _build/main.o _build/constants.o\""
    ,"$(LitE . StringL . loc_filename <$> location)"
    ]
