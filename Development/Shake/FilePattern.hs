{-# LANGUAGE PatternGuards #-}

module Development.Shake.FilePattern(
    FilePattern, (?==),
    compatible, extract, substitute,
    directories, directories1
    ) where

import System.FilePath(pathSeparators)
import Data.List
import Control.Arrow


---------------------------------------------------------------------
-- BASIC FILE PATTERN MATCHING

-- | A type synonym for file patterns, containing @\/\/@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
type FilePattern = String


data Lexeme = Star | SlashSlash | Char Char deriving (Show, Eq)

isChar (Char _) = True; isChar _ = False
isDull (Char x) = x /= '/'; isDull _ = False
fromChar (Char x) = x


data Regex = Lit [Char] | Not [Char] | Any
           | Start | End
           | Bracket Regex
           | Or Regex Regex | Concat Regex Regex
           | Repeat Regex | Empty
             deriving Show

type SString = (Bool, String) -- fst is True if at the start of the string


lexer :: FilePattern -> [Lexeme]
lexer ('*':xs) = Star : lexer xs
lexer ('/':'/':xs) = SlashSlash : lexer xs
lexer (x:xs) = Char x : lexer xs
lexer [] = []


pattern :: [Lexeme] -> Regex
pattern = Concat Start . foldr Concat End . map f
    where
        f Star = Bracket $ Repeat $ Not pathSeparators
        f SlashSlash = let s = Start `Or` End `Or` Lit pathSeparators in Bracket $
                       Or (s `Concat` Repeat Any `Concat` s) (Lit pathSeparators)
        f (Char x) = Lit $ if x == '/' then pathSeparators else [x]


-- | Return is (brackets, matched, rest)
match :: Regex -> SString -> [([String], String, SString)]
match (Lit l) (_, x:xs) | x `elem` l = [([], [x], (False, xs))]
match (Not l) (_, x:xs) | x `notElem` l = [([], [x], (False, xs))]
match Any (_, x:xs) = [([], [x], (False, xs))]
match Start (True, xs) = [([], [], (True, xs))]
match End (s, []) = [([], [], (s, []))]
match (Bracket r) xs = [(a ++ [b], b, c) | (a,b,c) <- match r xs]
match (Or r1 r2) xs = match r1 xs ++ match r2 xs
match (Concat r1 r2) xs = [(a1++a2,b1++b2,c2) | (a1,b1,c1) <- match r1 xs, (a2,b2,c2) <- match r2 c1]
match (Repeat r) xs = match (Empty `Or` Concat r (Repeat r)) xs
match Empty xs = [([], "", xs)]
match _ _ = []




-- | Match a 'FilePattern' against a 'FilePath', There are only two special forms:
--
-- * @*@ matches an entire path component, excluding any separators.
--
-- * @\/\/@ matches an arbitrary number of path components.
--
--   Some examples that match:
--
-- @
-- \"\/\/*.c\" '?==' \"foo\/bar\/baz.c\"
-- \"*.c\" '?==' \"baz.c\"
-- \"\/\/*.c\" '?==' \"baz.c\"
-- \"test.c\" '?==' \"test.c\"
-- @
--
--   Examples that /don't/ match:
--
-- @
-- \"*.c\" '?==' \"foo\/bar.c\"
-- \"*\/*.c\" '?==' \"foo\/bar\/baz.c\"
-- @
--
--   An example that only matches on Windows:
--
-- @
-- \"foo\/bar\" '?==' \"foo\\\\bar\"
-- @
(?==) :: FilePattern -> FilePath -> Bool
(?==) p x = not $ null $ match (pattern $ lexer p) (True, x)


---------------------------------------------------------------------
-- DIRECTORY PATTERNS

-- | Given a pattern, return the directory that requires searching,
--   with 'True' if it requires a recursive search. Must be conservative.
--   Examples:
--
-- > directories1 "*.xml" == ("",False)
-- > directories1 "//*.xml" == ("",True)
-- > directories1 "foo//*.xml" == ("foo",True)
-- > directories1 "foo/bar/*.xml" == ("foo/bar",False)
-- > directories1 "*/bar/*.xml" == ("",True)
directories1 :: FilePattern -> (FilePath, Bool)
directories1 = first (intercalate "/") . f . lexer
    where
        f xs | (a@(_:_),b:bs) <- span isDull xs, b `elem` [Char '/',SlashSlash] =
                if b == SlashSlash then ([map fromChar a],True) else first (map fromChar a:) $ f bs
             | all (\x -> isDull x || x == Star) xs = ([],False)
             | otherwise = ([], True)


-- | Given a set of patterns, produce a set of directories that require searching,
--   with 'True' if it requires a recursive search. Must be conservative. Examples:
--
-- > directories ["*.xml","//*.c"] == [("",True)]
-- > directories ["bar/*.xml","baz//*.c"] == [("bar",False),("baz",True)]
-- > directories ["bar/*.xml","baz//*.c"] == [("bar",False),("baz",True)]
directories :: [FilePattern] -> [(FilePath,Bool)]
directories ps = foldl f xs xs
    where
        xs = nub $ map directories1 ps 

        -- Eliminate anything which is a strict subset
        f xs (x,True) = filter (\y -> not $ (x,False) == y || (x ++ "/") `isPrefixOf` fst y) xs
        f xs _ = xs


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Do they have the same * and // counts in the same order
compatible :: [FilePattern] -> Bool
compatible [] = True
compatible (x:xs) = all ((==) (f x) . f) xs
    where f = filter (not . isChar) . lexer


-- | Extract the items that match the wildcards. The pair must match with '?=='.
extract :: FilePattern -> FilePath -> [String]
extract p x = ms
    where (ms,_,_):_ = match (pattern $ lexer p) (True,x)


-- | Given the result of 'extract', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substitute :: [String] -> FilePattern -> FilePath
substitute ms p = f ms (lexer p)
    where
        f ms (Char p:ps) = p : f ms ps
        f (m:ms) (_:ps) = m ++ f ms ps
        f [] [] = []
        f _ _ = error $ "Substitution failed into pattern " ++ show p ++ " with " ++ show (length ms) ++ " matches, namely " ++ show ms
