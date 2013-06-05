{-# LANGUAGE RecordWildCards, PatternGuards #-}

-- | The IO in this module is only to evaluate an envrionment variable,
--   the 'Env' itself it passed around purely.
module Development.Ninja.Type(
    Str, FileStr,
    Expr(..), Env, newEnv, askVar, askExpr, addEnv, addBind, addBinds,
    Ninja(..), newNinja, Build(..), Rule(..),
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import Data.Maybe


type Str = BS.ByteString
type FileStr = Str


---------------------------------------------------------------------
-- ENVIRONMENT

newtype Env = Env (Map.HashMap Str Str) deriving Show

data Expr = Exprs [Expr] | Lit Str | Var Str deriving Show

newEnv :: Env
newEnv = Env Map.empty

askExpr :: Env -> Expr -> Str
askExpr e = f
    where f (Exprs xs) = BS.concat $ map f xs
          f (Lit x) = x
          f (Var x) = askVar e x

askVar :: Env -> Str -> Str
askVar (Env e) x = fromMaybe BS.empty $ Map.lookup x e

addEnv :: Str -> Str -> Env -> Env
addEnv k v (Env e) = Env $ Map.insert k v e

addBind :: Str -> Expr -> Env -> Env
addBind k v env = addEnv k (askExpr env v) env

addBinds :: [(Str, Expr)] -> Env -> Env
addBinds kvs e = foldl (\e (k,v) -> addBind k v e) e kvs


---------------------------------------------------------------------
-- STRUCTURE

data Ninja = Ninja
    {defines :: !Env
    ,rules :: [(Str,Rule)]
    ,singles :: [(FileStr,Build)]
    ,multiples :: [([FileStr], Build)]
    ,phonys :: ([(Str, [FileStr])])
    ,defaults :: [FileStr]
    ,pools :: [(Str, Int)]
    }
    deriving Show

newNinja :: Ninja
newNinja = Ninja newEnv [] [] [] [] [] []

data Build = Build
    {ruleName :: Str
    ,depsNormal :: [FileStr]
    ,depsImplicit :: [FileStr]
    ,depsOrderOnly :: [FileStr]
    ,buildBind :: [(Str,Expr)]
    } deriving Show

data Rule = Rule
    {ruleBind :: [(Str,Expr)]
    } deriving Show
