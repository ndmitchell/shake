{-# LANGUAGE PatternGuards #-}

module Development.Ninja.Parse(parse) where

import Development.Ninja.Type
import Data.Char
import Data.List


parse :: FilePath -> IO [Stmt]
parse file = do
    src <- if file == "-" then getContents else readFile file
    return $ parseNinja src


parseNinja :: String -> [Stmt]
parseNinja xs = map (uncurry parseStmt) $ split $ filter (not . all isSpace) $ map comments $ continuations $ lines xs
    where
        continuations (x:y:xs) | "$" `isSuffixOf` x = continuations $ (init x ++ dropWhile isSpace y):xs
        continuations (x:xs) = x : continuations xs
        continuations [] = []

        comments = takeWhile (/= '#')

        split (x:xs) = (x,a) : split b
            where (a,b) = span (all isSpace . take 1) xs
        split [] = []


parseStmt :: String -> [String] -> Stmt
parseStmt stmt extra = case words stmt of
    var:"=":value -> Define var $ parseExpr value
    "rule":name:[] -> Rule name $ parseBinds extra
    "build":file:"phony":arg:[] -> Phony (init file) arg
    "build":file:name:arg -> Build (init file) name arg $ parseBinds extra
    "default":file -> Default file
    _ -> error $ "Unknown Ninja statement: " ++ stmt


parseBinds :: [String] -> [(String, Expr)]
parseBinds = map f
    where
        f x = case words x of
                  var:"=":value -> (var, parseExpr value)
                  _ -> error $ "Unknown Ninja binding: " ++ x


parseExpr :: [String] -> Expr
parseExpr = map Lit
