{-# LANGUAGE PatternGuards, RecordWildCards #-}

-- | Parsing is a slow point, the below is optimised
module Development.Ninja.Parse(parse) where

import qualified Data.ByteString.Char8 as BS
import Development.Ninja.Env
import Development.Ninja.Type
import Control.Monad
import Data.Char
import Data.Maybe


endsDollar :: Str -> Bool
endsDollar = BS.isSuffixOf (BS.pack "$")

dropSpace :: Str -> Str
dropSpace = BS.dropWhile isSpace

startsSpace :: Str -> Bool
startsSpace = BS.isPrefixOf (BS.pack " ")

-- | This is a hot-spot, so optimised
linesCR :: Str -> [Str]
linesCR x = case BS.split '\n' x of
    x:xs | Just ('\r',x) <- unsnoc x -> x : map (\x -> case unsnoc x of Just ('\r',x) -> x; _ -> x) xs
    xs -> xs
    where
        -- the ByteString unsnoc was introduced in a newer version
        unsnoc x | BS.null x = Nothing
                 | otherwise = Just (BS.last x, BS.init x)



word1 :: Str -> (Str, Str)
word1 x = (a, dropSpace b)
    where (a,b) = BS.break isSpace x

isVar :: Char -> Bool
isVar x = isAlphaNum x || x == '_'

parse :: FilePath -> IO Ninja
parse file = do env <- newEnv; parseFile file env newNinja


parseFile :: FilePath -> Env Str Str -> Ninja -> IO Ninja
parseFile file env ninja = do
    src <- if file == "-" then BS.getContents else BS.readFile file
    foldM (applyStmt env) ninja $ splitStmts src


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


applyStmt :: Env Str Str -> Ninja -> Stmt -> IO Ninja
applyStmt env ninja@Ninja{..} (Stmt x xs)
    | key == BS.pack "rule" =
        return ninja{rules = (BS.takeWhile isVar rest, Rule $ parseBinds xs) : rules}
    | key == BS.pack "default" = do
        xs <- parseStrs env rest
        return ninja{defaults = xs ++ defaults}
    | key == BS.pack "pool" = do
        depth <- getDepth env $ parseBinds xs
        return ninja{pools = (BS.takeWhile isVar rest, depth) : pools}
    | key == BS.pack "build" = do
        (out,rest) <- return $ splitColon rest
        outputs <- parseStrs env out
        (rule,deps) <- return $ word1 $ dropSpace rest
        (normal,implicit,orderOnly) <- fmap splitDeps $ parseStrs env deps
        let build = Build rule env normal implicit orderOnly $ parseBinds xs
        return $
            if rule == BS.pack "phony" then ninja{phonys = [(x, normal) | x <- outputs] ++ phonys}
            else if length outputs == 1 then ninja{singles = (head outputs, build) : singles}
            else ninja{multiples = (outputs, build) : multiples}
    | key == BS.pack "include" = parseFile (BS.unpack rest) env ninja
    | key == BS.pack "subninja" = do
        e <- scopeEnv env
        parseFile (BS.unpack rest) e ninja
    | Just (a,b) <- parseBind x = do
        addBind env a b
        return ninja
    | BS.null $ dropSpace x = return ninja
    | BS.pack "#" `BS.isPrefixOf` dropSpace x = return ninja -- comments can only occur on their own line
    | otherwise = error $ "Cannot parse line: " ++ BS.unpack x
    where (key,rest) = word1 x


getDepth :: Env Str Str -> [(Str, Expr)] -> IO Int
getDepth env xs = case lookup (BS.pack "depth") xs of
    Nothing -> return 1
    Just x -> do
        x <- askExpr env x
        case BS.readInt x of
            Just (i, n) | BS.null n -> return i
            _ -> error $ "Could not parse depth field in pool, got: " ++ BS.unpack x


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


parseStrs :: Env Str Str -> Str -> IO [Str]
parseStrs env = mapM (askExpr env) . parseExprs



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
