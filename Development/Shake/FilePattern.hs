{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.FilePattern(
    FilePattern, (?==)
    ) where

import Data.List

-- | A type synonym for file patterns, containing @\/\/@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
type FilePattern = String


-- | Match a 'FilePattern' against a 'FilePath', There are only two special forms:
--
-- * @*@ matches an entire path component, excluding any separators.
--
-- * @\/\/@ matches an arbitrary number of path componenets.
--
--   Some examples that match:
--
-- > "//*.c" ?== "foo/bar/baz.c"
-- > "*.c" ?== "baz.c"
-- > "//*.c" ?== "baz.c"
-- > "test.c" ?== "test.c"
--
--   Examples that /don't/ match:
--
-- > "*.c" ?== "foor/bar.c"
-- > "*/*.c" ?== "foo/bar/baz.c"
--
(?==) :: FilePattern -> FilePath -> Bool
(?==) ('/':'/':x) y = any (x ?==) $ y : [i | '/':i <- tails y]
(?==) ('*':x) y = any (x ?==) $ a ++ take 1 b
    where (a,b) = break ("/" `isPrefixOf`) $ tails y
(?==) (x:xs) (y:ys) | x == y = xs ?== ys
(?==) [] [] = True
(?==) _ _ = False
