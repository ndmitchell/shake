{-# LANGUAGE PatternGuards #-}

-- | Lexing is a slow point, the code below is optimised
module Development.Ninja.Lexer(Lexeme(..), lexer, lexerFile) where

import Control.Arrow
import qualified Data.ByteString.Char8 as BS
import Development.Ninja.Type
import Data.Char


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
dropSpace = BS.dropWhile (== ' ')

word1 :: Str -> (Str, Str)
word1 x = (a, dropSpace b)
    where (a,b) = BS.break (== ' ') x

isVar :: Char -> Bool
isVar x = isAlphaNum x || x == '_'


lexerFile :: Maybe FilePath -> IO [Lexeme]
lexerFile file = fmap lexer $ maybe BS.getContents BS.readFile file


lexer :: Str -> [Lexeme]
lexer o@x = case BS.uncons x of
    Nothing -> []
    Just (c,x) -> case c of
        '\r' -> lexer x
        '\n' -> lexer x
        ' ' -> lexBind $ BS.dropWhile (== ' ') x
        '#' -> lexer $ BS.dropWhile (/= '\n') x
        'b' | Just x <- strip "uild " x -> lexBuild $ dropSpace x
        'r' | Just x <- strip "ule " x -> lexRule $ dropSpace x
        'd' | Just x <- strip "efault " x -> lexDefault $ dropSpace x
        'p' | Just x <- strip "ool " x -> lexPool $ dropSpace x
        'i' | Just x <- strip "nclude " x -> lexInclude $ dropSpace x
        's' | Just x <- strip "ubninja " x -> lexSubninja $ dropSpace x
        _ -> lexDefine o
    where
        strip str x = if b `BS.isPrefixOf` x then Just $ BS.drop (BS.length b) x else Nothing
            where b = BS.pack str

lexBind o@x = case BS.uncons x of
    Nothing -> []
    Just (c,x) -> case c of
        '\r' -> lexer x
        '\n' -> lexer x
        '#' -> lexer $ BS.dropWhile (/= '\n') x
        _ -> lexxBind LexBind o

lexBuild x =
    let (this,next) = splitLineCont x
        (out,rest) = splitColon this
        outputs = parseExprs out
        (rule,deps) = word1 $ dropSpace rest
    in LexBuild outputs rule (parseExprs deps) : lexer next

lexDefault x
    | (files,rest) <- splitLineCont x
    = LexDefault (parseExprs files) : lexer rest

lexRule x = lexxName LexRule x
lexPool x = lexxName LexPool x
lexInclude x = lexxFile LexInclude x
lexSubninja x = lexxFile LexSubninja x
lexDefine x = lexxBind LexDefine x

lexxBind ctor x
    | (var,rest) <- BS.span isVar x
    , Just ('=',rest) <- BS.uncons $ dropSpace rest
    , (exp,rest) <- splitLineCont $ dropSpace rest
    = ctor var (parseExpr exp) : lexer rest
lexxBind _ x = error $ show ("parse failed when parsing binding", BS.take 100 x)

lexxFile ctor x
    | (file,rest) <- splitLineCont x
    = ctor file : lexer rest

lexxName ctor x
    | (name,rest) <- splitLineCont x
    = ctor name : lexer rest

splitLineCont :: Str -> (Str, Str)
splitLineCont x = first BS.concat $ f x
    where
        f x = if not $ endsDollar a then ([a], b) else let (c,d) = f $ dropSpace b in (BS.init a : c, d)
            where (a,b) = splitLineCR x

splitLineCR :: Str -> (Str, Str)
splitLineCR x = if BS.singleton '\r' `BS.isSuffixOf` a then (BS.init a, BS.drop 1 b) else (a, BS.drop 1 b)
    where (a,b) = BS.break (== '\n') x


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
