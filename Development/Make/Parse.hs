
module Development.Make.Parse(parse) where

import Development.Make.Type
import Data.Char
import Data.List


trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


parse :: FilePath -> IO Makefile
parse file = do
    src <- if file == "-" then getContents else readFile file
    return $ parseMakefile src


parseMakefile :: String -> Makefile
parseMakefile xs = Makefile $ rejoin $ concatMap parse $ map comments $ continuations $ lines xs
    where
        continuations (x:y:xs) | "\\" `isSuffixOf` x = continuations $ (init x ++ dropWhile isSpace y):xs
        continuations (x:xs) = x : continuations xs
        continuations [] = []

        comments = takeWhile (/= '#')

        parse x | all isSpace x = []
                | all isSpace $ take 1 x = [Right $ parseCommand $ trim x]
                | (a,b) <- break (== ';') x = Left (parseStmt a) : [Right $ parseCommand $ trim $ drop 1 b | b /= ""]

        rejoin (Left r@Rule{}:Right e:xs) = rejoin $ Left r{commands = commands r ++ [e]} : xs
        rejoin (Right e:xs) = error $ "Command must be under a rule: " ++ show e
        rejoin (Left r:xs) = r : rejoin xs
        rejoin [] = []


parseStmt :: String -> Stmt
parseStmt x
    | (a,'=':b) <- break (== '=') x, ':' `notElem` a =
        if "+" `isSuffixOf` a then Assign (trim $ init a) PlusEquals (parseExpr $ trim b)
        else if "?" `isSuffixOf` a then Assign (trim $ init a) QuestionEquals (parseExpr $ trim b)
        else Assign (trim a) Equals (parseExpr $ trim b)
    | (a,':':b) <- break (== ':') x = case b of
        '=':b -> Assign (trim a) ColonEquals (parseExpr $ trim b)
        ':':'=':b -> Assign (trim a) ColonEquals (parseExpr $ trim b)
        _ -> Rule (parseExpr $ trim a) (parseExpr $ trim b) []
    | otherwise = error $ "Invalid statement: " ++ x


parseExpr :: String -> Expr
parseExpr x = simplifyExpr $ Concat $ f x
    where
        f ('$':'$':x) = Lit "$" : f x
        f ('$':'(':xs) = case break (== ')') xs of
            (var,')':rest) -> parseVar var : f rest
            _ -> error $ "Couldn't find trailing `)' after " ++ xs
        f ('$':'{':xs) = case break (== '}') xs of
            (var,'}':rest) -> parseVar var : f rest
            _ -> error $ "Couldn't find trailing `}' after " ++ xs
        f ('$':x:xs) = Var [x] : f xs
        f (x:xs) = Lit [x] : f xs
        f [] = []


parseVar :: String -> Expr
parseVar x = Var x


parseCommand :: String -> Command
parseCommand = Expr . parseExpr
