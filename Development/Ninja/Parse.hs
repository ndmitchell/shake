{-# LANGUAGE RecordWildCards #-}

-- | Parsing is a slow point, the below is optimised
module Development.Ninja.Parse(parse) where

import qualified Data.ByteString.Char8 as BS
import Development.Ninja.Env
import Development.Ninja.Type
import Development.Ninja.Lexer
import Control.Monad


parse :: FilePath -> IO Ninja
parse file = do env <- newEnv; parseFile file env newNinja


parseFile :: FilePath -> Env Str Str -> Ninja -> IO Ninja
parseFile file env ninja = do
    lexes <- lexerFile $ if file == "-" then Nothing else Just file
    foldM (applyStmt env) ninja $ withBinds lexes

withBinds :: [Lexeme] -> [(Lexeme, [(Str,Expr)])]
withBinds [] = []
withBinds (x:xs) = (x,a) : withBinds b
    where
        (a,b) = f xs
        f (LexBind a b : rest) = let (as,bs) = f rest in ((a,b):as, bs)
        f xs = ([], xs)


applyStmt :: Env Str Str -> Ninja -> (Lexeme, [(Str,Expr)]) -> IO Ninja
applyStmt env ninja@Ninja{..} (key, binds) = case key of
    LexBuild outputs rule deps -> do
        outputs <- mapM (askExpr env) outputs
        deps <- mapM (askExpr env) deps
        let (normal,implicit,orderOnly) = splitDeps deps
        let build = Build rule env normal implicit orderOnly binds
        return $
            if rule == BS.pack "phony" then ninja{phonys = [(x, normal) | x <- outputs] ++ phonys}
            else if length outputs == 1 then ninja{singles = (head outputs, build) : singles}
            else ninja{multiples = (outputs, build) : multiples}
    LexRule name ->
        return ninja{rules = (name, Rule binds) : rules}
    LexDefault xs -> do
        xs <- mapM (askExpr env) xs
        return ninja{defaults = xs ++ defaults}
    LexPool name -> do
        depth <- getDepth env binds
        return ninja{pools = (name, depth) : pools}
    LexInclude file ->
        parseFile (BS.unpack file) env ninja
    LexSubninja file -> do
        e <- scopeEnv env
        parseFile (BS.unpack file) e ninja
    LexDefine a b -> do
        addBind env a b
        return ninja
    LexBind a b ->
        error $ "Unexpected binding defining " ++ BS.unpack a


splitDeps :: [Str] -> ([Str], [Str], [Str])
splitDeps (x:xs) | x == BS.pack "|" = ([],a++b,c)
                 | x == BS.pack "||" = ([],b,a++c)
                 | otherwise = (x:a,b,c)
    where (a,b,c) = splitDeps xs
splitDeps [] = ([], [], [])


getDepth :: Env Str Str -> [(Str, Expr)] -> IO Int
getDepth env xs = case lookup (BS.pack "depth") xs of
    Nothing -> return 1
    Just x -> do
        x <- askExpr env x
        case BS.readInt x of
            Just (i, n) | BS.null n -> return i
            _ -> error $ "Could not parse depth field in pool, got: " ++ BS.unpack x
