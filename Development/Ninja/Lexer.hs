{-# LANGUAGE PatternGuards #-}

-- | Parsing is a slow point, the below is optimised
module Development.Ninja.Lexer(Lexeme(..), lexer, lexerFile) where

import qualified Data.ByteString.Char8 as BS
import Development.Shake.ByteString
import Development.Ninja.Type
import Data.Char
import Data.Maybe


-- Lex each line separately, rather than each lexeme
data Lexeme
    = LexBind Str Expr -- [indent]foo = bar
    | LexBuild [Expr] Str [Expr] -- build foo: bar | baz || qux (| and || are represented as Expr)
    | LexInclude Str -- include file
    | LexSubninja Str -- include file
    | LexRule Str -- rule name
    | LexPool Str -- pool name
    | LexDefault [Expr] -- default foo bar
    | LexDefine Str Expr -- foo = bar

endsDollar :: Str -> Bool
endsDollar = BS.isSuffixOf (BS.singleton '$')

dropSpace :: Str -> Str
dropSpace = BS.dropWhile isSpace

startsSpace :: Str -> Bool
startsSpace = BS.isPrefixOf (BS.singleton ' ')

word1 :: Str -> (Str, Str)
word1 x = (a, dropSpace b)
    where (a,b) = BS.break isSpace x

isVar :: Char -> Bool
isVar x = isAlphaNum x || x == '_'


lexerFile :: Maybe FilePath -> IO [Lexeme]
lexerFile file = fmap lexer $ maybe BS.getContents BS.readFile file


lexer :: Str -> [Lexeme]
lexer = concatMap f . splitStmts
    where f (Stmt x xs) = maybeToList (asKeyword x) ++ mapMaybe asBind xs


data Stmt = Stmt Str [Str] deriving Show

splitStmts :: Str -> [Stmt]
splitStmts = stmt . continuation . linesCR
    where
        continuation (x:xs) | endsDollar x = BS.concat (BS.init x : map (dropSpace . BS.init) a ++ map dropSpace (take 1 b)) : continuation (drop 1 b)
            where (a,b) = span endsDollar xs
        continuation (x:xs) = x : continuation xs
        continuation [] = []

        stmt [] = []
        stmt (x:xs) = Stmt x (map dropSpace a) : stmt b
            where (a,b) = span startsSpace xs


asKeyword :: Str -> Maybe Lexeme
asKeyword x
    | BS.null $ dropSpace x = Nothing
    | BS.pack "#" `BS.isPrefixOf` dropSpace x = Nothing -- comments can only occur on their own line
    | key == BS.pack "build" =
        let (out,rest2) = splitColon rest
            outputs = parseExprs out
            (rule,deps) = word1 $ dropSpace rest2
        in Just $ LexBuild outputs rule $ parseExprs deps
    | key == BS.pack "rule" =
        Just $ LexRule $ BS.takeWhile isVar rest
    | key == BS.pack "default" =
        Just $ LexDefault $ parseExprs rest
    | key == BS.pack "pool" =
        Just $ LexPool $ BS.takeWhile isVar rest
    | key == BS.pack "include" =
        Just $ LexInclude rest
    | key == BS.pack "subninja" =
        Just $ LexSubninja rest
    | Just (a,b) <- parseBind x = do
        Just $ LexDefine a b
    | otherwise = error $ "Cannot parse line: " ++ BS.unpack x
    where (key,rest) = word1 x

parseBind :: Str -> Maybe (Str, Expr)
parseBind x
    | (var,rest) <- BS.span isVar x
    , Just ('=',rest) <- BS.uncons $ dropSpace rest
    = Just (var, parseExpr $ dropSpace rest)
    | otherwise = Nothing


asBind :: Str -> Maybe Lexeme
asBind x = Just $ uncurry LexBind $ fromMaybe (error $ "Unknown Ninja binding: " ++ BS.unpack x) $ parseBind x


parseExpr :: Str -> Expr
parseExpr = exprs . f
    where
        exprs [x] = x
        exprs xs = Exprs xs

        f x = case BS.elemIndex '$' x of
            Nothing -> [Lit x]
            Just i -> Lit (BS.take i x) : g (BS.drop (i+1) x)

        g x = case BS.uncons x of
            Nothing -> []
            Just (c,s)
                | c == '$' -> Lit (BS.singleton '$') : f s
                | c == ' ' -> Lit (BS.singleton ' ') : f s
                | c == ':' -> Lit (BS.singleton ':') : f s
                | c == '{' -> let (a,b) = BS.break (== '}') s in Var a : f (BS.drop 1 b)
                | otherwise -> let (a,b) = BS.span isVar x in Var a : f b


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
