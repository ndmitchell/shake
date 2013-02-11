{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- | This module provides versions of the 'Development.Shake.system'' family of functions
--   which take a variable number of arguments.
--
--   All these functions take a variable number of arguments.
--
-- * @String@ arguments are treated as whitespace separated arguments.
--
-- * @[String]@ arguments are treated as literal arguments.
--
--   As an example, to run @ghc --make -O2 inputs -o output@:
--
-- @
--   'sys' \"ghc --make -O2\" inputs \"-o\" [output]
-- @
--
--   Note that we enclose @output@ as a list so that if the output name contains spaces they are
--   appropriately escaped.
module Development.Shake.Sys(
    sys, sysCwd, sysOutput, args
    ) where
    
import Development.Shake.Core
import Development.Shake.Derived


type VarArgs a t = a 


-- | A variable arity version of 'system''.
sys :: SysArguments v => String -> VarArgs v (Action ())
sys x = sys_ [] x

class SysArguments t where sys_ :: [String] -> t
instance (Arg a, SysArguments r) => SysArguments (a -> r) where
    sys_ xs x = sys_ $ xs ++ arg x
instance SysArguments (Action ()) where
    sys_ (x:xs) = system' x xs
    sys_ [] = error "No executable or arguments given to sys"


-- | A variable arity version of 'systemCwd'.
sysCwd :: SysCwdArguments v => FilePath -> String -> VarArgs v (Action ())
sysCwd dir x = sysCwd_ dir [] x

class SysCwdArguments t where sysCwd_ :: FilePath -> [String] -> t
instance (Arg a, SysCwdArguments r) => SysCwdArguments (a -> r) where
    sysCwd_ dir xs x = sysCwd_ dir $ xs ++ arg x
instance SysCwdArguments (Action ()) where
    sysCwd_ dir (x:xs) = systemCwd dir x xs
    sysCwd_ dir [] = error "No executable or arguments given to sysCwd"


-- | A variable arity version of 'systemOutput'.
sysOutput :: SysOutputArguments v => String -> VarArgs v (Action (String, String))
sysOutput x = sysOutput_ [] x

class SysOutputArguments t where sysOutput_ :: [String] -> t
instance (Arg a, SysOutputArguments r) => SysOutputArguments (a -> r) where
    sysOutput_ xs x = sysOutput_ $ xs ++ arg x
instance SysOutputArguments (Action (String, String)) where
    sysOutput_ (x:xs) = systemOutput x xs
    sysOutput_ [] = error "No executable or arguments given to sys"


-- | A variable arity function to accumulate a list of arguments.
args :: ArgsArguments v => VarArgs v [String]
args = args_ []

class ArgsArguments t where args_ :: [String] -> t
instance (Arg a, ArgsArguments r) => ArgsArguments (a -> r) where
    args_ xs x = args_ $ xs ++ arg x
instance ArgsArguments [String] where
    args_ = id


class Arg a where arg :: a -> [String]
instance Arg String where arg = words
instance Arg [String] where arg = id
