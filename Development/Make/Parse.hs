
module Development.Make.Parse(parse) where

import Development.Make.Type
import Data.Char


trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


parse :: FilePath -> IO Makefile
parse file = do
    src <- readFile file
    return $ parseMakefile src


parseMakefile :: String -> Makefile
parseMakefile xs = Makefile $ rejoin $ concatMap parse $ lines xs
    where
        parse x | all isSpace x = []
                | all isSpace $ take 1 x = [Right $ parseCommand $ trim x]
                | otherwise = [Left $ parseStmt x]

        rejoin (Left r@Rule{}:Right e:xs) = rejoin $ Left r{commands = commands r ++ [e]} : xs
        rejoin (Right e:xs) = error $ "Command must be under a rule: " ++ show e
        rejoin (Left r:xs) = r : rejoin xs
        rejoin [] = []


parseStmt :: String -> Stmt
parseStmt x
    | (a,'=':b) <- break (== '=') x, ':' `notElem` a = Assign Equals (trim a) (parseExpr $ trim b)
    | (a,':':b) <- break (== ':') x = Rule (parseExpr $ trim a) (parseExpr $ trim b) []
    | otherwise = error $ "Invalid statement: " ++ x


parseExpr :: String -> Expr
parseExpr x = simplifyExpr $ Concat $ f x
    where
        f ('$':'$':x) = Lit "$" : f x
        f ('$':'(':xs) = case break (== ')') xs of
            (var,')':rest) -> parseVar var : f rest
            _ -> error $ "Couldn't find trailing `)' after " ++ xs
        f ('$':x:xs) = Var [x] : f xs
        f (x:xs) = Lit [x] : f xs
        f [] = []


parseVar :: String -> Expr
parseVar x = Var x


parseCommand :: String -> Command
parseCommand = Expr . parseExpr
