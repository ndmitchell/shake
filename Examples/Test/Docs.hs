{-# LANGUAGE PatternGuards #-}

module Examples.Test.Docs(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Exit


reps from to = map (\x -> if x == from then to else x)

main = shaken noTest $ \args obj -> do
    let index = "dist/doc/html/shake/index.html"
    want [obj "Success.txt"]

    want $ map (\x -> fromMaybe (obj x) $ stripPrefix "!" x) args

    let needSource = need =<< getDirectoryFiles "." ["Development/Shake.hs","Development/Shake//*.hs","Development/Ninja/*.hs","General//*.hs"]

    index *> \_ -> do
        needSource
        need ["shake.cabal"]
        trackAllow ["dist//*"]
        res <- liftIO $ findExecutable "cabal"
        if isJust res then cmd "cabal haddock" else do
            Exit exit <- cmd "runhaskell Setup.hs haddock"
            when (exit /= ExitSuccess) $ do
                () <- cmd "runhaskell Setup.hs configure"
                cmd "runhaskell Setup.hs haddock"

    obj "Paths_shake.hs" *> \out -> do
        copyFile' "Paths.hs" out

    obj "Part_*.hs" *> \out -> do
        need ["Examples/Test/Docs.hs"] -- so much of the generator is in this module
        src <- if "_md" `isSuffixOf` takeBaseName out then
            fmap (findCodeMarkdown . lines) $ readFile' $ "docs/" ++ drop 5 (reverse (drop 3 $ reverse $ takeBaseName out)) ++ ".md"
         else
            fmap findCodeHaddock $ readFile' $ "dist/doc/html/shake/" ++ reps '_' '-' (drop 5 $ takeBaseName out) ++ ".html"
        let f i (Stmt x) | whitelist $ head x = []
                         | otherwise = restmt i $ map undefDots $ trims x
            f i (Expr x) | takeWhile (not . isSpace) x `elem` types = ["type Expr_" ++ show i ++ " = " ++ x]
                         | "import " `isPrefixOf` x = [x]
                         | otherwise = ["expr_" ++ show i ++ " = (" ++ undefDots x2 ++ ")" | let x2 = trim $ dropComment x, not $ whitelist x2]
            code = concat $ zipWith f [1..] (nub src)
            (imports,rest) = partition ("import " `isPrefixOf`) code
        writeFileLines out $
            ["{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, ExtendedDefaultRules, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}"
            ,"{-# OPTIONS_GHC -w #-}"
            ,"module " ++ takeBaseName out ++ "() where"
            ,"import Control.Concurrent"
            ,"import Control.Monad"
            ,"import Data.Char"
            ,"import Data.Data"
            ,"import Data.List"
            ,"import Data.Maybe"
            ,"import Data.Monoid"
            ,"import Development.Shake"
            ,"import Development.Shake.Classes"
            ,"import Development.Shake.Rule"
            ,"import Development.Shake.Util"
            ,"import Development.Shake.FilePath"
            ,"import System.Console.GetOpt"
            ,"import System.Exit"
            ,"import System.IO"] ++
            ["import " ++ reps '_' '.' (drop 5 $ takeBaseName out) | not $ "_md.hs" `isSuffixOf` out] ++
            imports ++
            ["(==>) :: Bool -> Bool -> Bool"
            ,"(==>) = undefined"
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
            ,"str1 = \"\""
            ,"str2 = \"\""
            ,"str = \"\""
            ,"((/./),(/../),(//)) = undefined"] ++
            rest

    obj "Files.lst" *> \out -> do
        need ["Examples/Test/Docs.hs"] -- so much of the generator is in this module
        need [index,obj "Paths_shake.hs"]
        filesHs <- getDirectoryFiles "dist/doc/html/shake" ["Development-*.html"]
        filesMd <- getDirectoryFiles "docs" ["*.md"]
        writeFileChanged out $ unlines $
            ["Part_" ++ reps '-' '_' (takeBaseName x) | x <- filesHs, not $ "-Classes.html" `isSuffixOf` x] ++
            ["Part_" ++ takeBaseName x ++ "_md" | x <- filesMd]

    let needModules = do mods <- readFileLines $ obj "Files.lst"; need [obj m <.> "hs" | m <- mods]; return mods

    obj "Main.hs" *> \out -> do
        mods <- needModules
        writeFileLines out $ ["module Main(main) where"] ++ ["import " ++ m | m <- mods] ++ ["main = return ()"]

    obj "Success.txt" *> \out -> do
        needModules
        need [obj "Main.hs", obj "Paths_shake.hs"]
        needSource
        () <- cmd "runhaskell -ignore-package=hashmap" ["-i" ++ obj "", obj "Main.hs"]
        writeFile' out ""


data Code = Stmt [String] | Expr String deriving (Show,Eq)

findCodeHaddock :: String -> [Code]
findCodeHaddock x | Just x <- stripPrefix "<pre>" x = f (Stmt . shift . lines . strip) "</pre>" x
                  | Just x <- stripPrefix "<code>" x = f (Expr . strip) "</code>" x
    where
        f ctor end x | Just x <- stripPrefix end x = ctor "" : findCodeHaddock x
        f ctor end (x:xs) = f (ctor . (x:)) end xs
findCodeHaddock (x:xs) = findCodeHaddock xs
findCodeHaddock [] = []

findCodeMarkdown :: [String] -> [Code]
findCodeMarkdown (x:xs) | indented x && not (blank x) =
    let (a,b) = span (\x -> indented x || blank x) (x:xs)
    in Stmt (map (drop 4) a) : findCodeMarkdown b
    where
        indented x = length (takeWhile isSpace x) >= 4
        blank x = all isSpace x
findCodeMarkdown (x:xs) = f x ++ findCodeMarkdown xs
    where
        f ('`':xs) = let (a,b) = break (== '`') xs in Expr a : f (drop 1 b)
        f (x:xs) = f xs
        f [] = []
findCodeMarkdown [] = []

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
trims = reverse . dropWhile (all isSpace) . reverse . dropWhile (all isSpace)

restmt i ("":xs) = restmt i xs
restmt i (x:xs) | " ?== " `isInfixOf` x || " == " `isInfixOf` x =
    zipWith (\j x -> "hack_" ++ show i ++ "_" ++ show j ++ " = " ++ x) [1..] (x:xs)
restmt i (x:xs) |
    not ("let" `isPrefixOf` x) && not ("[" `isPrefixOf` x) && (" = " `isInfixOf` x || " | " `isInfixOf` x) ||
    "import " `isPrefixOf` x || "infix" `isPrefixOf` x || "instance " `isPrefixOf` x = map f $ x:xs
    where f x = if takeWhile (not . isSpace) x `elem` dupes then "_" ++ show i ++ "_" ++ x else x
restmt i xs = ("stmt_" ++ show i ++ " = do") : map ("  " ++) xs ++
              ["  undefined" | length xs == 1 && ("let" `isPrefixOf` (head xs) || "<-" `isInfixOf` (head xs))]


shift :: [String] -> [String]
shift xs | all null xs = xs
         | all (\x -> null x || " " `isPrefixOf` x) xs = shift $ map (drop 1) xs
         | otherwise = xs


dropComment ('-':'-':_) = []
dropComment xs = onTail dropComment xs


undefDots o = f o
    where
        f ('.':'.':'.':xs) =
            (if "cmd" `elem` words o then "[\"\"]" else "undefined") ++
            (if "..." `isSuffixOf` xs then "" else undefDots xs)
        f xs = onTail f xs

strip :: String -> String
strip x
    | Just x <- stripPrefix "<em>" x
    , (a,b) <- break (== '<') x
    , not $ ("</em>" `isPrefixOf` b) && a `elem` italics
    = error $ "Unexpected italics in code block: " ++ a ++ take 5 b ++ "..."
strip ('<':xs) = strip $ drop 1 $ dropWhile (/= '>') xs
strip ('&':xs)
    | Just xs <- stripPrefix "quot;" xs = '\"' : strip xs
    | Just xs <- stripPrefix "lt;" xs = '<' : strip xs
    | Just xs <- stripPrefix "gt;" xs = '>' : strip xs
    | Just xs <- stripPrefix "amp;" xs = '&' : strip xs
strip xs = onTail strip xs

onTail f (x:xs) = x : f xs
onTail f [] = []


italics :: [String]
italics = words "extension command-name file-name"

whitelist :: String -> Bool
whitelist x | all (not . isSpace) x && takeExtension x `elem` words ".txt .hi .hs .o .exe .tar .cpp .cfg .dep .deps .h .c .html" = True
whitelist x | elem x $ words $
    "newtype do MyFile.txt.digits excel a q m c x value key gcc cl os make contents tar ghc cabal clean _make distcc ghc " ++
    ".. /./ /.. ./ // \\ ../ " ++
    "ConstraintKinds GeneralizedNewtypeDeriving DeriveDataTypeable SetConsoleTitle " ++
    "Data.List System.Directory Development.Shake.FilePath main.m run .rot13 " ++
    "NoProgress Error src rot13 .js .json " ++
    ".make/i586-linux-gcc/output _make/.database foo/.. file.src file.out build " ++
    "/usr/special /usr/special/userbinary $CFLAGS %PATH% -O2 -j8 -j -j1 " ++
    "-threaded -rtsopts -I0 Function extension $OUT $C_LINK_FLAGS $PATH xterm $TERM main opts result flagValues argValues " ++
    "HEADERS_DIR /path/to/dir CFLAGS let -showincludes -MMD gcc.version linkFlags temp pwd touch code out err " ++
    "_metadata/.database _shake _shake/build ./build.sh build.sh build.bat //* [out] manual/examples.zip manual " ++
    "docs/manual _build _build/run ninja depfile build.ninja "
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
    ,"# This is my Config file"
    ,"-g -I/path/to/dir -O2"
    ,"main _make/henry.txt"
    ,"<i>build rules</i>"
    ,"<i>actions</i>"
    ,"() <- cmd ..."
    ,"x <- inputs"
    ,"shakeFiles=\"_build/\""
    ,"#include \""
    ,"pattern *> actions = (pattern ?==) ?> actions" -- because it overlaps
    ,"buildDir = \"_build\""
    ,"-MMD -MF"
    ,"#!/bin/sh"
    ,"build _build/main.o"
    ,"build clean"
    ,"build -j8"
    ,"runhaskell _build/run"
    ,"runhaskell _build/run clean"
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
    ]

types = words $
    "MVar IO Monad Monoid String FilePath Data Maybe [String] Eq Typeable Char ExitCode Change " ++
    "Action Resource Assume FilePattern Lint Verbosity Rules Rule CmdOption CmdResult Int Double"

dupes = words "main progressSimple rules"
