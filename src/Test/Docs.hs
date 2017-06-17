{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Test.Docs(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Test.Type
import Control.Monad
import Data.Char
import Data.List.Extra
import Data.Maybe
import System.Info
import Data.Version.Extra


-- Older versions of Haddock garbage the --@ markup and have ambiguity errors
brokenHaddock = compilerVersion < makeVersion [8]

main = shakeTest_ (unless brokenHaddock . noTest) $ do
    let index = "dist/doc/html/shake/index.html"
    let config = "dist/setup-config"
    want ["Success.txt"]

    let needSource = need =<< getDirectoryFiles "." (map (root </>)
            ["src/Development/Shake.hs","src/Development/Shake//*.hs","src/Development/Ninja/*.hs","src/General//*.hs"])

    config %> \_ -> do
        need $ map (root </>) ["shake.cabal","Setup.hs"]
        -- Make Cabal and Stack play nicely
        path <- getEnv "GHC_PACKAGE_PATH"
        liftIO $ createDirectoryIfMissing True "dist"
        dist <- liftIO $ canonicalizePath "dist" -- make sure it works even if we cwd
        cmd_ (RemEnv "GHC_PACKAGE_PATH") (Cwd root) "runhaskell Setup.hs configure"
            ["--builddir=" ++ dist,"--user"]
            -- package-db is very sensitive, see #267
            ["--package-db=" ++ x | x <- maybe [] (filter (`notElem` [".",""]) . splitSearchPath) path]
        trackAllow ["dist//*"]

    index %> \_ -> do
        need $ config : map (root </>) ["shake.cabal","Setup.hs","README.md","CHANGES.txt"]
        needSource
        trackAllow ["dist//*"]
        dist <- liftIO $ canonicalizePath "dist"
        cmd (Cwd root) "runhaskell Setup.hs haddock" ["--builddir=" ++ dist]

    "Paths_shake.hs" %> \out ->
        copyFile' (root </> "src/Paths.hs") out

    "Part_*.hs" %> \out -> do
        need [root </> "src/Test/Docs.hs"] -- so much of the generator is in this module
        let noR = filter (/= '\r')
        src <- if "_md" `isSuffixOf` takeBaseName out then
            fmap (findCodeMarkdown . lines . noR) $ readFile' $ root </> "docs/" ++ drop 5 (reverse (drop 3 $ reverse $ takeBaseName out)) ++ ".md"
         else
            fmap (findCodeHaddock . noR) $ readFile' $ root </> "dist/doc/html/shake/" ++ replace "_" "-" (drop 5 $ takeBaseName out) ++ ".html"

        let (imports,rest) = partition ("import " `isPrefixOf`) $ showCode src
        writeFileChanged out $ unlines $
            ["{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}"
            ,"{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, TypeFamilies #-}"
            ,"{-# OPTIONS_GHC -w #-}"
            ,"module " ++ takeBaseName out ++ "() where"
            ,"import Control.Applicative"
            ,"import Control.Concurrent"
            ,"import Control.Exception"
            ,"import Control.Monad"
            ,"import Data.ByteString(ByteString)"
            ,"import Data.Char"
            ,"import Data.Data"
            ,"import Data.Dynamic"
            ,"import Data.List.Extra"
            ,"import System.Time.Extra"
            ,"import Data.Maybe"
            ,"import Data.Monoid"
            ,"import Development.Shake hiding ((*>),trackAllow)"
            ,"import Development.Shake.Classes"
            ,"import Development.Shake.Rule hiding (trackAllow)"
            ,"import Development.Shake.Util"
            ,"import Development.Shake.FilePath"
            ,"import System.Console.GetOpt"
            ,"import System.Directory(setCurrentDirectory)"
            ,"import qualified System.Directory"
            ,"import System.Environment(lookupEnv)"
            ,"import System.Process"
            ,"import System.Exit"
            ,"import Control.Applicative"
            ,"import Control.Monad.IO.Class"
            ,"import System.IO"] ++
            ["import " ++ replace "_" "." (drop 5 $ takeBaseName out) | not $ "_md.hs" `isSuffixOf` out] ++
            imports ++
            ["(==>) :: Bool -> Bool -> Bool"
            ,"(==>) = undefined"
            ,"(<==) = ()"
            ,"infix 1 ==>"
            ,"infix 0 ==="
            ,"(===) :: a -> a -> b"
            ,"(===) = undefined"
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
            ,"(p1,p2) = (0.0, 0.0)"
            ,"(r1,r2) = (return () :: Rules(), return () :: Rules())"
            ,"xs = []"
            ,"ys = []"
            ,"out = \"\""
            ,"str1 = \"\""
            ,"str2 = \"\""
            ,"str = \"\""] ++
            rest

    "Files.lst" %> \out -> do
        need [root </> "src/Test/Docs.hs"] -- so much of the generator is in this module
        need [index,"Paths_shake.hs"]
        filesHs <- getDirectoryFiles (root </> "dist/doc/html/shake") ["Development-*.html"]
        filesMd <- getDirectoryFiles (root </> "docs") ["*.md"]
        writeFileChanged out $ unlines $
            ["Part_" ++ replace "-" "_" (takeBaseName x) | x <- filesHs, not $ "-Classes.html" `isSuffixOf` x] ++
            ["Part_" ++ takeBaseName x ++ "_md" | x <- filesMd, takeBaseName x `notElem` ["Developing","Model"]]

    let needModules = do mods <- readFileLines "Files.lst"; need [m <.> "hs" | m <- mods]; return mods

    "Main.hs" %> \out -> do
        mods <- needModules
        writeFileLines out $ ["module Main(main) where"] ++ ["import " ++ m | m <- mods] ++ ["main = return ()"]

    "Success.txt" %> \out -> do
        needModules
        need ["Main.hs", "Paths_shake.hs"]
        needSource
        cmd_ "runhaskell -ignore-package=hashmap " ["-i.","-i" ++ root </> "src","Main.hs"]
        writeFile' out ""


---------------------------------------------------------------------
-- FIND THE CODE

newtype Code = Code [String] deriving (Show,Eq,Ord)


findCodeHaddock :: String -> [Code]
findCodeHaddock src =
    [ Code $ unindent $ lines $ innerText x
    | tag <- ["code","pre"]
    , x <- insideTag tag src
    , let bad = nubOrd (insideTag "em" x) \\ italics
    , if null bad then True else error $ "Bad italics, " ++ show bad]


findCodeMarkdown :: [String] -> [Code]
findCodeMarkdown (x:xs) | indented x && not (isBlank x) =
    let (a,b) = span (\x -> indented x || isBlank x) (x:xs)
    in Code (unindent a) : findCodeMarkdown b
    where
        indented x = length (takeWhile isSpace x) >= 4
findCodeMarkdown (x:xs) = map (Code . return) (evens $ splitOn "`" x) ++ findCodeMarkdown xs
    where
        evens (x:y:xs) = y : evens xs
        evens _ = []
findCodeMarkdown [] = []


---------------------------------------------------------------------
-- RENDER THE CODE

showCode :: [Code] -> [String]
showCode = concat . zipWith f [1..] . nubOrd
    where
        f i (Code x) | "#" `isPrefixOf` concat x = []
                     | all whitelist x = []
                     | otherwise = showStmt i $ filter (not . isBlank . dropComment) $ map (fixCmd . undefDots) x


fixCmd :: String -> String
fixCmd x | "cmd " `isPrefixOf` x || "command " `isPrefixOf` x = "unit $ " ++ x
fixCmd x = replace "Stdout out" "Stdout (out :: String)" $ replace "Stderr err" "Stderr (err :: String)" x

-- | Replace ... with undefined (don't use undefined with cmd; two ...'s should become one replacement)
undefDots :: String -> String
undefDots x | Just x <- stripSuffix "..." x, Just (x,_) <- stripInfix "..." x = x ++ new
            | otherwise = replace "..." new x
    where new = if words x `disjoint` ["cmd","Development.Shake.cmd"] then "undefined" else "[\"\"]"

showStmt :: Int -> [String] -> [String]
showStmt i [] = []
showStmt i xs | isDecl $ unlines xs = map f xs
    where f x = if fst (word1 x) `elem` dupes then "_" ++ show i ++ "_" ++ x else x
showStmt i (x:xs) | fst (word1 x) `elem` types = ["type Code_" ++ show i ++ " = " ++ x]
showStmt i [x] | length (words x) <= 2 = ["code_" ++ show i ++ " = (" ++ x ++ ")"] -- deal with operators and sections
showStmt i xs | all isPredicate xs, length xs > 1 =
    zipWith (\j x -> "code_" ++ show i ++ "_" ++ show j ++ " = " ++ x) [1..] xs
showStmt i xs = ("code_" ++ show i ++ " = do") : map ("  " ++) xs ++ ["  undefined" | isBindStmt $ last xs]

isPredicate :: String -> Bool
isPredicate x = not $ disjoint (words x) ["==","?=="]

isBindStmt :: String -> Bool
isBindStmt x = "let " `isPrefixOf` x || " <- " `isInfixOf` x

isDecl :: String -> Bool
isDecl x | fst (word1 x) `elem` ["import","infix","instance","newtype"] = True
isDecl (words -> name:"::":_) | all isAlphaNum name = True -- foo :: Type Signature
isDecl x | "=" `elem` takeWhile (`notElem` ["let","where"]) (words $ takeWhile (/= '{') x) = True -- foo arg1 arg2 = an implementation
isDecl _ = False


---------------------------------------------------------------------
-- TEXT MANIPULATION

-- | Is a string empty or whitespace
isBlank :: String -> Bool
isBlank = all isSpace

-- | If all lines are indented by at least n spaces, then trim n spaces from each line
unindent :: [String] -> [String]
unindent xs = map (drop n) xs
    where n = minimum $ 1000 : map (length . takeWhile (== ' ')) (filter (not . isBlank) xs)

-- | Remove line comments from the end of lines
dropComment :: String -> String
dropComment = fst . breakOn "--"

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
    "Action Resource Rebuild FilePattern Development.Shake.FilePattern " ++
    "Lint Verbosity Rules CmdOption Int Double " ++
    "NFData Binary Hashable Eq Typeable Show Applicative " ++
    "CmdResult ByteString ProcessHandle Rule Monad Monoid Data TypeRep " ++
    "BuiltinRun BuiltinLint"

-- | Duplicated identifiers which require renaming
dupes :: [String]
dupes = words "main progressSimple rules"


isFilePath :: String -> Bool
isFilePath x = all validChar  x && ("foo/" `isPrefixOf` x || takeExtension x `elem` exts)
    where
        validChar x = isAlphaNum x || x `elem` "_./*"
        exts = words $ ".txt .hi .hs .o .exe .tar .cpp .cfg .dep .out .deps .m .h .c .html .zip " ++
                       ".js .json .trace .database .src .sh .bat .ninja .rot13 .version .digits .prof .md"

isCmdFlag :: String -> Bool
isCmdFlag "+RTS" = True
isCmdFlag x = length a `elem` [1,2] && all (\x -> isAlphaNum x || x `elem` "-=/_[]") b
    where (a,b) = span (== '-') x

isCmdFlags :: String -> Bool
isCmdFlags = all (\x -> let y = fromMaybe x $ stripSuffix "," x in isCmdFlag y || isArg y) . words
    where isArg = all (\x -> isUpper x || x == '=')

isEnvVar :: String -> Bool
isEnvVar x | Just x <- stripPrefix "$" x = all validChar x
           | Just x <- stripPrefix "%" x, Just x <- stripSuffix "%" x = all validChar x
           | otherwise = False
    where validChar x = isAlpha x || x == '_'

isProgram :: String -> Bool
isProgram (words -> x:xs) = x `elem` programs && all (\x -> isCmdFlag x || isFilePath x || all isAlpha x || x == "&&") xs
    where programs = words "excel gcc cl make ghc cabal distcc build tar git fsatrace ninja touch pwd runhaskell rot13 main shake stack"

-- | Should a fragment be whitelisted and not checked
whitelist :: String -> Bool
whitelist x | null x || isFilePath x || isCmdFlags x || isEnvVar x || isProgram x = True
whitelist x | elem x $ words $
    "newtype do a q m c x value key os contents clean _make " ++
    ".. /. // \\ //* dir/*/* dir " ++
    "ConstraintKinds TemplateHaskell GeneralizedNewtypeDeriving DeriveDataTypeable TypeFamilies SetConsoleTitle " ++
    "Data.List System.Directory Development.Shake.FilePath run " ++
    "NoProgress Error src about://tracing " ++
    ".make/i586-linux-gcc/output build " ++
    "/usr/special /usr/special/userbinary " ++
    "Hidden extension xterm main opts result flagValues argValues " ++
    "HEADERS_DIR /path/to/dir CFLAGS let linkFlags temp code out err " ++
    "_shake _shake/build manual chrome://tracing/ compdb " ++
    "docs/manual foo.* _build _build/run depfile 0.000s " ++
    "@ndm_haskell file-name .PHONY filepath trim base stack extra #include " ++
    "*> BuiltinRun BuiltinLint RuleResult"
    = True
whitelist x = x `elem`
    ["[Foo.hi, Foo.o]"
    ,"shake-progress"
    ,"type instance"
    ,"1m25s (15%)"
    ,"3m12s (82%)"
    ,"getPkgVersion $ GhcPkgVersion \"shake\""
    ,"ghc --make MyBuildSystem -threaded -rtsopts \"-with-rtsopts=-I0 -qg qb\""
    ,"# command-name (for file-name)"
    ,"<i>build rules</i>"
    ,"<i>actions</i>"
    ,"shakeFiles=\"_build\""
    ,"#include \""
    ,"pattern %> actions = (pattern ?==) ?> actions" -- because it overlaps
    ,"buildDir = \"_build\""
    ,"#!/bin/sh"
    ,"shake-build-system"
    ,"\"_build\" </> x -<.> \"o\""
    ,"cmd \"gcc -o\" [out] os"
    ,"[item1,item2,item2]"
    ,"$(LitE . StringL . loc_filename <$> location)"
    ,"-d[ FILE], --debug[=FILE]"
    ,"-r[ FILE], --report[=FILE], --profile[=FILE]"
    ]
