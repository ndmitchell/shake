{-# LANGUAGE PatternGuards, RecordWildCards #-}

-- | Parsing is a slow point, the below is optimised
module Development.Ninja.Parse(parse) where

import qualified Data.ByteString.Char8 as BS
import Development.Ninja.Type
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe


endsDollar :: Str -> Bool
endsDollar = BS.isSuffixOf (BS.pack "$")

dropSpace :: Str -> Str
dropSpace = BS.dropWhile isSpace

startsSpace :: Str -> Bool
startsSpace = BS.isPrefixOf (BS.pack " ")

dropEndCR :: Str -> Str
dropEndCR x = if BS.isSuffixOf (BS.pack "\r") x then BS.init x else x

word1 :: Str -> (Str, Str)
word1 x = (a, dropSpace b)
    where (a,b) = BS.break isSpace x

isVar :: Char -> Bool
isVar x = isAlphaNum x || x == '_'

parse :: FilePath -> IO Ninja
parse file = parseFile file newNinja


parseFile :: FilePath -> Ninja -> IO Ninja
parseFile file ninja = do
    src <- if file == "-" then BS.getContents else BS.readFile file
    foldM applyStmt ninja $ splitStmts src


data Stmt = Stmt Str [Str] deriving Show


splitStmts :: Str -> [Stmt]
splitStmts = stmt . continuation . map dropEndCR . BS.lines
    where
        continuation (x:xs) | endsDollar x = BS.concat (BS.init x : map (dropSpace . BS.init) a ++ map dropSpace (take 1 b)) : continuation (drop 1 b)
            where (a,b) = span endsDollar xs
        continuation (x:xs) = x : continuation xs
        continuation [] = []

        stmt [] = []
        stmt (x:xs) = Stmt x (map dropSpace a) : stmt b
            where (a,b) = span startsSpace xs


applyStmt :: Ninja -> Stmt -> IO Ninja
applyStmt ninja@Ninja{..} (Stmt x xs)
    | key == BS.pack "rule" =
        return ninja{rules = (BS.takeWhile isVar rest, Rule $ parseBinds xs) : rules}
    | key == BS.pack "default" =
        return ninja{defaults = parseStrs defines rest ++ defaults}
    | key == BS.pack "pool" =
        return ninja{pools = (BS.takeWhile isVar rest, getDepth $ map (second $ askExpr defines) $ parseBinds xs) : pools}
    | key == BS.pack "build",
        (out,rest) <- splitColon rest,
        outputs <- parseStrs defines out,
        (rule,deps) <- word1 $ dropSpace rest,
        (normal,implicit,orderOnly) <- splitDeps $ parseStrs defines deps,
        let build = Build rule normal implicit orderOnly $ parseBinds xs
        = return $
            if rule == BS.pack "phony" then ninja{phonys = [(x, normal) | x <- outputs] ++ phonys}
            else if length outputs == 1 then ninja{singles = (head outputs, build) : singles}
            else ninja{multiples = (outputs, build) : multiples}
    | key == BS.pack "include" = parseFile (BS.unpack rest) ninja
    | key == BS.pack "subninja" = do
        ninja <- parseFile (BS.unpack rest) ninja
        -- restore to the original environment after, which is what Ninja actually does
        return ninja{defines = defines}
    | Just (a,b) <- parseBind x = return ninja{defines = addBind a b defines}
    | BS.null $ dropSpace x = return ninja
    | BS.pack "#" `BS.isPrefixOf` dropSpace x = return ninja -- comments can only occur on their own line
    | otherwise = error $ "Cannot parse line: " ++ BS.unpack x
    where (key,rest) = word1 x


getDepth :: [(Str, Str)] -> Int
getDepth xs = case lookup (BS.pack "depth") xs of
    Nothing -> 1
    Just x | Just (i, n) <- BS.readInt x, BS.null n -> i
           | otherwise -> error $ "Could not parse depth field in pool, got: " ++ BS.unpack x


parseBind :: Str -> Maybe (Str, Expr)
parseBind x
    | (var,rest) <- BS.span isVar x
    , Just ('=',rest) <- BS.uncons $ dropSpace rest
    = Just (var, parseExpr $ dropSpace rest)
    | otherwise = Nothing


parseBinds :: [Str] -> [(Str, Expr)]
parseBinds = map $ \x -> fromMaybe (error $ "Unknown Ninja binding: " ++ BS.unpack x) $ parseBind x


parseExpr :: Str -> Expr
parseExpr = exprs . f
    where
        exprs [x] = x
        exprs xs = Exprs xs

        f x = Lit a : g (BS.drop 1 b)
            where (a,b) = BS.break (== '$') x

        g x = case BS.uncons x of
            Nothing -> []
            Just (c,s)
                | c == '$' -> Lit (BS.singleton '$') : f s
                | c == ' ' -> Lit (BS.singleton ' ') : f s
                | c == ':' -> Lit (BS.singleton ':') : f s
                | c == '{' -> let (a,b) = BS.break (== '}') s in Var a : f (BS.drop 1 b)
                | otherwise -> let (a,b) = BS.span isVar x in Var a : f b


parseStrs :: Env -> Str -> [Str]
parseStrs env = map (askExpr env) . parseExprs



splitDeps :: [Str] -> ([Str], [Str], [Str])
splitDeps (x:xs) | x == BS.pack "|" = ([],a++b,c)
                 | x == BS.pack "||" = ([],b,a++c)
                 | otherwise = (x:a,b,c)
    where (a,b,c) = splitDeps xs
splitDeps [] = ([], [], [])


parseExprs :: Str -> [Expr]
parseExprs = map parseExpr . f
    where
        f x | BS.null x = []
            | otherwise = let (a,b) = splitUnescaped ' ' x in a : f b


splitUnescaped :: Char -> Str -> (Str, Str)
splitUnescaped c x = case filter valid $ BS.elemIndices c x of
    [] -> (x, BS.empty)
    i:_ -> (BS.take i x, BS.drop (i+1) x)
    where
        -- technically there could be $$:, so an escaped $ followed by a literal :
        -- that seems very unlikely...
        valid i = i == 0 || BS.index x (i-1) /= '$'

-- Find a non-escape : 
splitColon :: Str -> (Str, Str)
splitColon = splitUnescaped ':'
