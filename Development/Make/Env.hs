
-- | The IO in this module is only to evaluate an envrionment variable,
--   the 'Env' itself it passed around purely.
module Development.Make.Env(Env, newEnv, addEnv, askEnv) where

import Development.Make.Type
import Data.Maybe
import qualified Data.HashMap.Strict as Map


newtype Env = Env (Map.HashMap String (Assign,Expr))

newEnv :: [(String,String)] -> Env
newEnv xs = Env $ Map.fromList [(a,(Equals,Lit b)) | (a,b) <- xs]


addEnv :: String -> Assign -> Expr -> Env -> IO Env
addEnv name ass val env@(Env e) = case ass of
    QuestionEquals -> if isJust $ Map.lookup name e then return env else addEnv name Equals val env
    Equals -> return $ Env $ Map.insert name (Equals,val) e
    ColonEquals -> do l <- askEnv env val; return $ Env $ Map.insert name (ColonEquals,Lit l) e
    PlusEquals -> case Map.lookup name e of
        Just (Equals,x) -> return $ Env $ Map.insert name (Equals,Concat [x,Lit " ",val]) e
        Just (ColonEquals,x) -> do l <- askEnv env val; return $ Env $ Map.insert name (ColonEquals,Concat [x,Lit " ",Lit l]) e
        _ -> addEnv name Equals val env


askEnv :: Env -> Expr -> IO String
askEnv (Env e) x = do
    res <- f [] x
    case simplifyExpr res of
        Lit x -> return x
        x -> error $ "Internal error in askEnv, " ++ show x
    where
        f seen (Var x) | x `elem` seen = error $ "Recursion in variables, " ++ show seen
                       | Just (_,y) <- Map.lookup x e = f (x:seen) y
                       | otherwise = return $ Lit ""
        f seen x = descendExprM (f seen) x

