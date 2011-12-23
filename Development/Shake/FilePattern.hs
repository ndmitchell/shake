{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.FilePattern(
    FilePattern, (?==),
    compatible, extract, substitute
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
(?==) ('/':'/':p) x = any (p ?==) $ x : [i | '/':i <- tails x]
(?==) ('*':p) x = any (p ?==) $ a ++ take 1 b
    where (a,b) = break ("/" `isPrefixOf`) $ tails x
(?==) (p:ps) (x:xs) | p == x = ps ?== xs
(?==) [] [] = True
(?==) _ _ = False


-- | Do they have the same * and // counts in the same order
compatible :: [FilePattern] -> Bool
compatible [] = True
compatible (x:xs) = all ((==) (f x) . f) xs
    where
        f ('*':xs) = '*':f xs
        f ('/':'/':xs) = '/':f xs
        f (x:xs) = f xs
        f [] = []


-- | Extract the items that match the wildcards. The pair must match with '?=='.
extract :: FilePattern -> FilePath -> [String]
extract p x = head $ f p x ++ [[]]
    where
        f ('/':'/':p) x = rest p $ ("",x) : [(pre++"/",i) | (pre,'/':i) <- zip (inits x) (tails x)]
        f ('*':p) x = rest p $ a ++ take 1 b
            where (a,b) = break (isPrefixOf "/" . snd) $ zip (inits x) (tails x)
        f (p:ps) (x:xs) | p == x = f ps xs
        f [] [] = [[]]
        f _ _ = []

        rest p xs = [(skip:res) | (skip,keep) <- xs, res <- f p keep]


-- | Given the result of 'extract', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substitute :: [String] -> FilePattern -> FilePath
substitute = f
    where
        f (a:as) ('/':'/':ps) = a ++ f as ps
        f (a:as) ('*':ps) = a ++ f as ps
        f as (p:ps) = p : f as ps
        f as [] = []
