{-# LANGUAGE PatternGuards #-}

module Development.Ninja.Parse(parse) where

import Development.Ninja.Type
import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe


parse :: FilePath -> IO [Stmt]
parse file = do
    src <- if file == "-" then getContents else readFile file
    parseStmts src


parseStmts :: String -> IO [Stmt]
parseStmts xs = mapM (uncurry parseStmt) $ split $ filter (not . all isSpace) $ map comments $ continuations $ lines xs
    where
        continuations (x:y:xs) | "$" `isSuffixOf` x = continuations $ (init x ++ dropWhile isSpace y):xs
        continuations (x:xs) = x : continuations xs
        continuations [] = []

        comments = takeWhile (/= '#')

        split (x:xs) = (x,map (dropWhile isSpace) a) : split b
            where (a,b) = span (all isSpace . take 1) xs
        split [] = []


parseStmt :: String -> [String] -> IO Stmt
parseStmt stmt extra
    | Just (var,val) <- parseBind stmt = return $ Define var val
    | ("rule",name) <- splitWhite stmt = return $ Rule name $ parseBinds extra
    | ("default",files) <- splitWhite stmt = return $ Default $ parseExprs files
    | ("pool",name) <- splitWhite stmt = return $ Pool name $ lookup "depth" $ parseBinds extra
    | ("build",rest) <- splitWhite stmt,
           (out,rest) <- splitColon rest,
           (rule,rest) <- splitWhite $ dropWhile isSpace rest
           = return $ Build (parseExprs out) rule (parseExprs rest) (parseBinds extra)
    | ("subninja",file) <- splitWhite stmt = fmap Scope $ parse file
    | otherwise = error $ "Unknown Ninja statement: " ++ stmt


splitWhite :: String -> (String, String)
splitWhite x = (a, dropWhile isSpace b)
    where (a,b) = break isSpace x

splitColon :: String -> (String, String)
splitColon (':':xs) = ("", xs)
splitColon ('$':':':xs) = first ("$:"++) $ splitColon xs
splitColon (x:xs) = first (x:) $ splitColon xs
splitColon [] = ("","")


parseBind :: String -> Maybe (String, Expr)
parseBind x
    | (var,rest) <- splitWhite x, ("=",value) <- splitWhite rest = Just (var, parseExpr value)
    | otherwise = Nothing


parseBinds :: [String] -> [(String, Expr)]
parseBinds = map $ \x -> fromMaybe (error $ "Unknown Ninja binding: " ++ x) $ parseBind x


parseExpr :: String -> Expr
parseExpr = split
    where
        lit s (Lit x:xs) = Lit (s:x) : xs
        lit s xs = Lit [s] : xs

        split ('$':'$':xs) = lit '$' $ split xs
        split ('$':' ':xs) = lit ' ' $ split xs
        split ('$':':':xs) = lit ':' $ split xs
        split ('$':'{':xs) = Var a : split (drop 1 b)
            where (a,b) = break (== '}') xs
        split ('$':xs) = Var a : split b
            where (a,b) = span (\x -> isAlphaNum x || x == '_') xs
        split (x:xs) = lit x $ split xs
        split [] = []


parseExprs :: String -> [Expr]
parseExprs = map parseExpr . split
    where
        split [] = []
        split (x:xs) | isSpace x = [] : split (dropWhile isSpace xs)
        split ('$':' ':xs) = add "$ " xs
        split ('$':'$':xs) = add "$$" xs
        split (x:xs) = add [x] xs

        add x xs = case split xs of
            [] -> [x]
            a:b -> (x++a):b
