{-# LANGUAGE PatternGuards #-}

module Development.Ninja.Type where


data Stmt
    = Rule
        {name :: String
        ,bind :: [(String, Expr)]
        }
    | Define
        {name :: String
        ,value :: Expr
        }
    | Build
        {output :: [Expr]
        ,rule :: String
        ,inputs :: [Expr]
        ,bind :: [(String, Expr)]
        }
    | Default
        {target :: [Expr]}
    | Pool
        {name :: String
        ,depth :: Maybe Expr}
    | Scope
        {nested :: [Stmt]}
      deriving Show

type Expr = [Lexeme]

data Lexeme = Lit String | Var String
    deriving Show
