
module Development.Make.Type where

import Control.Monad


data Makefile = Makefile [Stmt] deriving Show

data Stmt
    = Rule
        {targets :: Expr
        ,prerequisites :: Expr
        ,commands :: [Command]
        }
    | Assign
        {name :: String
        ,assign :: Assign
        ,expr :: Expr
        }
      deriving Show

data Assign = Equals | ColonEquals | QuestionEquals deriving Show

data Expr = Apply String [Expr]
          | Concat [Expr]
          | Var String
          | Lit String
            deriving Show

data Command = Expr Expr | String := Expr deriving Show

descendExpr :: (Expr -> Expr) -> Expr -> Expr
descendExpr f (Apply a b) = Apply a $ map f b
descendExpr f (Concat xs) = Concat $ map f xs
descendExpr f x = x

descendExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
descendExprM f (Apply a b) = Apply a `liftM` mapM f b
descendExprM f (Concat xs) = Concat `liftM` mapM f xs
descendExprM f x = return x

transformExpr :: (Expr -> Expr) -> Expr -> Expr
transformExpr f = f . descendExpr (transformExpr f)

simplifyExpr :: Expr -> Expr
simplifyExpr = transformExpr f
    where
        f (Concat xs) = case g xs of
            [] -> Lit ""
            [x] -> x
            xs -> Concat xs
        f x = x

        g (Concat x:xs) = g $ x ++ xs
        g (Lit x:Lit y:xs) = g $ Lit (x ++ y) : xs
        g (x:xs) = x : g xs
        g [] = []


substitute :: [(String, Expr)] -> Expr -> Expr
substitute vars = simplifyExpr . transformExpr (f [])
    where
        f seen (Var x) | x `elem` seen = error $ "Recursion in variables, " ++ show seen
                       | Just y <- lookup x vars = f (x:seen) y
        f seen x = descendExpr (f seen) x

