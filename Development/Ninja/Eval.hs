{-# LANGUAGE RecordWildCards, PatternGuards #-}

-- | The IO in this module is only to evaluate an envrionment variable,
--   the 'Env' itself it passed around purely.
module Development.Ninja.Eval(
    Env, addEnv, askEnv, var,
    Ninja(..), Builder(..),
    eval
    ) where

import qualified Data.HashMap.Strict as Map
import Development.Ninja.Type
import Data.Maybe


newtype Env = Env (Map.HashMap String String) deriving Show

addEnv :: String -> String -> Env -> Env
addEnv name val (Env mp) = Env $ Map.insert name val mp


var :: String -> Expr
var x = [Var x]


askEnv :: Env -> Expr -> String
askEnv (Env mp) = concatMap f
    where f (Lit x) = x
          f (Var x) = fromMaybe "" $ Map.lookup x mp


data Ninja = Ninja
    {rules :: Map.HashMap String [(String, Expr)]
    ,defines :: Env
    ,singles :: Map.HashMap FilePath Builder
    ,multiples :: Map.HashMap FilePath ([FilePath], Builder)
    ,phonys :: [(String, [FilePath])]
    ,defaults :: [FilePath]
    ,pools :: Map.HashMap String Int
    }
    deriving Show

ninja0 = Ninja Map.empty (Env Map.empty) Map.empty Map.empty [] [] Map.empty

data Builder = Builder
    {ruleName :: String
    ,deps:: [FilePath]
    ,impDeps :: [FilePath]
    ,ordDeps :: [FilePath]
    ,bindings :: [(String,Expr)]
    } deriving Show


eval :: [Stmt] -> Ninja
eval = foldl f ninja0
    where
        g env = askEnv (defines env)

        f env Rule{..} = env{rules = Map.insert name bind $ rules env}
        f env Define{..} = env{defines = addEnv name (g env value) $ defines env}
        f env Default{..} = env{defaults = map (g env) target ++ defaults env}
        f env Pool{..} = env{pools = Map.insert name (maybe 1 (read . g env) depth) $ pools env}
        f env Scope{..} = foldl f env nested
        f env Build{..}
            | rule == "phony" = env{phonys = [(o,a) | o <- os] ++ phonys env}
            | [o] <- os = env{singles = Map.insert o builder $ singles env}
            | otherwise = env{multiples = foldl (\mp o -> Map.insert o (os,builder) mp) (multiples env) os}
            where
                (a,b,c) = splitDeps $ map (g env) inputs
                builder = Builder rule a b c bind
                os = map (g env) output


splitDeps :: [String] -> ([String], [String], [String])
splitDeps (x:xs) | x == "|" = ([],a++b,c)
                 | x == "||" = ([],b,a++c)
                 | otherwise = (x:a,b,c)
    where (a,b,c) = splitDeps xs
splitDeps [] = ([], [], [])
