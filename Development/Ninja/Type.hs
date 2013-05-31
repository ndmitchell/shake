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
        {output :: FilePath
        ,rule :: String
        ,inputs :: [FilePath]
        ,bind :: [(String, Expr)]
        }
    | Phony
        {name :: String
        ,alias :: FilePath
        }
    | Default
        {target :: [FilePath]}
    | Pool
        {name :: String
        ,depth :: Int}
      deriving Show

type Expr = [Lexeme]

data Lexeme = Lit String | Var String
    deriving Show
