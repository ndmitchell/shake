{-# LANGUAGE RecordWildCards #-}

-- | The IO in this module is only to evaluate an envrionment variable,
--   the 'Env' itself it passed around purely.
module Development.Ninja.Eval(Ninja(..), eval) where

import qualified Data.HashMap.Strict as Map
import Development.Ninja.Type
import Data.Maybe


data Ninja = Ninja
    {rules :: Map.HashMap String [(String, Expr)]
    ,defines :: Map.HashMap String String
    ,builds :: Map.HashMap FilePath Stmt -- Stmt is of type Build
    ,phonys :: [(String, FilePath)]
    ,defaults :: [FilePath]
    ,pools :: Map.HashMap String Int
    }
    deriving Show

ninja0 = Ninja Map.empty Map.empty Map.empty [] [] Map.empty

eval :: [Stmt] -> Ninja
eval = foldl f ninja0
    where
        f env Rule{..} = env{rules = Map.insert name bind $ rules env}
        f env Define{..} = env{defines = Map.insert name (resolve (defines env) value) $ defines env}
        f env s@Build{..} = env{builds = Map.insert output s $ builds env}
        f env Phony{..} = env{phonys = (name,alias) : phonys env}
        f env Default{..} = env{defaults = target ++ defaults env}
        f env Pool{..} = env{pools = Map.insert name depth $ pools env}


resolve :: Map.HashMap String String -> Expr -> String
resolve env = concatMap f
    where f (Lit x) = x
          f (Var x) = fromMaybe "" $ Map.lookup x env
