{-# LANGUAGE PatternGuards #-}

module Examples.Test.Docs(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import Data.Char
import Data.List
import Data.Maybe


reps from to = map (\x -> if x == from then to else x)

main = shaken noTest $ \args obj -> do
    let index = "dist/doc/html/shake/index.html"
    want [obj "Success.txt"]

    want $ map (\x -> fromMaybe (obj x) $ stripPrefix "!" x) args

    index *> \_ -> do
        xs <- getDirectoryFiles "Development" ["//*.hs"]
        need $ map ("Development" </>) xs
        cmd "cabal haddock"

    obj "Paths_shake.hs" *> \out -> do
        copyFile' "Paths.hs" out

    obj "Part_*.hs" *> \out -> do
        need ["Examples/Test/Docs.hs"] -- so much of the generator is in this module
        src <- readFile' $ "dist/doc/html/shake/" ++ reps '_' '-' (drop 5 $ takeBaseName out) ++ ".html"
        let f i (Stmt x) | all whitelist x = []
                         | otherwise = restmt i $ map undefDots x
            f i (Expr x) | x `elem` types = ["type Expr_" ++ show i ++ " = " ++ x]
                         | otherwise = ["expr_" ++ show i ++ " = (" ++ undefDots x2 ++ ")" | let x2 = trim $ dropComment x, not $ whitelist x2]
            code = concat $ zipWith f [1..] (nub $ findCode src)
            (imports,rest) = partition ("import " `isPrefixOf`) code
        writeFileLines out $
            ["{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, ExtendedDefaultRules, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}"
            ,"module " ++ takeBaseName out ++ "() where"
            ,"import Control.Concurrent"
            ,"import Control.Monad"
            ,"import Data.Char"
            ,"import Data.Data"
            ,"import Data.List"
            ,"import Data.Monoid"
            ,"import Development.Shake"
            ,"import Development.Shake.Classes"
            ,"import System.Console.GetOpt"
            ,"import System.Exit"
            ,"import System.IO"
            ,"import " ++ reps '_' '.' (drop 5 $ takeBaseName out)
            ] ++
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
            ,"opts = shakeOptions"
            ,"result = undefined :: IO (Maybe (Rules ()))"
            ,"instance Eq (OptDescr a)"
            ,"inputs = [\"\"]"
            ,"output = \"\""
            ,"(foo,bar,baz) = undefined"
            ,"((/./),(/../),(//)) = undefined"
            ] ++
            rest

    obj "Main.hs" *> \out -> do
        need [index,obj "Paths_shake.hs"]
        files <- getDirectoryFiles "dist/doc/html/shake" ["Development-*.html"]
        files <- return $ filter (not . isSuffixOf "-Classes.html") files
        let mods = map ((++) "Part_" . reps '-' '_' . takeBaseName) files
        need [obj m <.> "hs" | m <- mods]
        writeFileLines out $ ["module Main(main) where"] ++ ["import " ++ m | m <- mods] ++ ["main = return ()"]

    obj "Success.txt" *> \out -> do
        need [obj "Main.hs"]
        () <- cmd "runhaskell -ignore-package=hashmap" ["-i" ++ obj "", obj "Main.hs"]
        writeFile' out ""


data Code = Stmt [String] | Expr String deriving (Show,Eq)

findCode :: String -> [Code]
findCode x | Just x <- stripPrefix "<pre>" x = f (Stmt . shift . lines . strip) "</pre>" x
           | Just x <- stripPrefix "<code>" x = f (Expr . strip) "</code>" x
    where
        f ctor end x | Just x <- stripPrefix end x = ctor "" : findCode x
        f ctor end (x:xs) = f (ctor . (x:)) end xs
findCode (x:xs) = findCode xs
findCode [] = []

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

restmt i ("":xs) = restmt i xs
restmt i (x:xs) | " ?== " `isInfixOf` x || " == " `isInfixOf` x =
    zipWith (\j x -> "hack_" ++ show i ++ "_" ++ show j ++ " = " ++ x) [1..] (x:xs)
restmt i (x:xs) | " = " `isInfixOf` x || " | " `isInfixOf` x ||
    "import " `isPrefixOf` x || "infix" `isPrefixOf` x || "instance " `isPrefixOf` x = map f $ x:xs
    where f x = if takeWhile (not . isSpace) x `elem` dupes then "_" ++ show i ++ "_" ++ x else x
restmt i xs = ("stmt_" ++ show i ++ " = do") : map ("  " ++) xs


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
whitelist x | takeExtension x `elem` words ".txt .hi .o .exe .tar .cpp" = True
whitelist x | elem x $ words $
    "newtype do MyFile.txt.digits excel a q value key gcc make contents tar ghc cabal clean _make distcc ghc " ++
    ".. /./ /.. ./ // \\ ../ " ++
    "ConstraintKinds GeneralizedNewtypeDeriving DeriveDataTypeable SetConsoleTitle " ++
    "NoProgress Error " ++
    ".make/i586-linux-gcc/output _make/.database foo/.. file.src file.out " ++
    "-threaded -rtsopts -I0 Function extension $OUT $PATH xterm $TERM main opts result flagValues argValues "
    = True
whitelist x = x `elem`
    ["[Foo.hi, Foo.o]"
    ,"shake-progress"
    ,"main -j6"
    ,"main clean"
    ,"1m25s (15%)"
    ,"getPkgVersion $ GhcPkgVersion \"shake\""
    ,"# command-name file-name"
    ,"ghc --make MyBuildSystem -rtsopts \"-with-rtsopts=-I0 -qg -qb\""
    ,"-qg -qb"
    ]

types = words $
    "MVar IO Monad Monoid String FilePath Data [String] Eq Typeable Char ExitCode " ++
    "Action Resource Assume FilePattern Verbosity Rules Rule CmdOption CmdResult Int Double"

dupes = words "main progressSimple rules"
