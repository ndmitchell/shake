{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Development.Shake.FilePattern(
    -- * Primitive API, as exposed
    FilePattern, (?==), (<//>),
    -- * Optimisation opportunities
    simple,
    -- * Multipattern file rules
    compatible, extract, substitute,
    -- * Accelerated searching
    directories,
    -- * Testing only
    directories1,
    internalTest
    ) where

import Development.Shake.FilePatternOld hiding ((?==))
import System.FilePath(isPathSeparator)
import Data.List.Extra
import Control.Applicative
import Control.Monad
import Prelude


data Pat = Lit String -- foo
         | Star   -- /*/
         | Skip -- //
         | Skip1 -- //, but must be at least 1 element
         | Stars String [String] String -- *foo*, prefix (fixed), infix floaters, suffix
                                        -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq)


data Lexeme = Str String | Slash | SlashSlash

lexer :: FilePattern -> [Lexeme]
lexer "" = []
lexer (x1:x2:xs) | isPathSeparator x1, isPathSeparator x2 = SlashSlash : lexer xs
lexer (x1:xs) | isPathSeparator x1 = Slash : lexer xs
lexer xs = Str a : lexer b
    where (a,b) = break isPathSeparator xs


-- | Parse a FilePattern. All optimisations I can think of are invalid because they change the extracted expressions.
parse :: FilePattern -> [Pat]
parse = f False True . lexer
    where
        -- str = I have ever seen a Str go past (equivalent to "can I be satisfied by no paths")
        -- slash = I am either at the start, or my previous character was Slash
        f str slash [] = [Lit "" | slash]
        f str slash (Str x:xs) = parseLit x : f True False xs
        f str slash (SlashSlash:Slash:xs) | not str = Skip1 : f str True xs
        f str slash (SlashSlash:xs) = Skip : f str False xs
        f str slash (Slash:xs) = [Lit "" | not str] ++ f str True xs


parseLit :: String -> Pat
parseLit "*" = Star
parseLit x = case split (== '*') x of
    [x] -> Lit x
    pre:xs | Just (mid,post) <- unsnoc xs -> Stars pre mid post


internalTest :: IO ()
internalTest = do
    let x # y = when (parse x /= y) $ fail $ show ("FilePattern.internalTest",x,parse x,y)
    "" # [Lit ""]
    "/" # [Lit "",Lit ""]
    "x/" # [Lit "x",Lit ""]
    "/x" # [Lit "",Lit "x"]
    "x/y" # [Lit "x",Lit "y"]
    "//" # [Skip]
    "//x" # [Skip, Lit "x"]
    "x//" # [Lit "x", Skip]
    "x//y" # [Lit "x",Skip, Lit "y"]
    "///" # [Skip1, Lit ""]
    "///x" # [Skip1, Lit "x"]
    "x///" # [Lit "x", Skip, Lit ""]
    "x///y" # [Lit "x",Skip, Lit "y"]
    "////" # [Skip, Skip]
    "////x" # [Skip, Skip, Lit "x"]
    "x////" # [Lit "x", Skip, Skip]
    "x////y" # [Lit "x",Skip, Skip, Lit "y"]


-- | Optimisations that may change the matched expressions
optimise :: [Pat] -> [Pat]
optimise (Skip:Skip:xs) = optimise $ Skip:xs
optimise (Skip:Star:xs) = optimise $ Skip1:xs
optimise (Star:Skip:xs) = optimise $ Skip1:xs
optimise (x:xs) = x : optimise xs
optimise [] =[]


match :: [Pat] -> [String] -> [[String]]
match (Skip:xs) (y:ys) = map ("":) (match xs (y:ys)) ++ match (Skip1:xs) (y:ys)
match (Skip1:xs) (y:ys) = [(if null r then y else y ++ "/" ++ r):rs | r:rs <- match (Skip:xs) ys]
match (Skip:xs) [] = map ("":) $ match xs []
match (Star:xs) (y:ys) = map (y:) $ match xs ys
match (Lit x:xs) (y:ys) | x == y = match xs ys
match (x@Stars{}:xs) (y:ys) | Just rs <- matchStars x y = map (rs++) $ match xs ys
match [] [] = [[]]
match _ _ = []

-- Only return the first (all patterns left-most) valid star matching
matchStars :: Pat -> String -> Maybe [String]
matchStars (Stars pre mid post) x = do
    x <- stripPrefix pre x
    x <- if null post then Just x else stripSuffix post x
    stripInfixes mid x
    where
        stripInfixes [] x = Just [x]
        stripInfixes (m:ms) x = do
            (a,x) <- stripInfix m x
            (a:) <$> stripInfixes ms x


(?==) :: FilePattern -> FilePath -> Bool
(?==) p = case optimise $ parse p of
    [Skip] -> const True
    [Skip1] -> const True
    p -> not . null . match p . split isPathSeparator


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

specials :: FilePattern -> String
specials ('*':xs) = '*' : specials xs
specials (x1:x2:xs) | isPathSeparator x1, isPathSeparator x2 = '/':'/': specials xs
specials (x:xs) = specials xs
specials [] = []

-- | Is the pattern free from any * and //.
simple :: FilePattern -> Bool
simple = null . specials

-- | Do they have the same * and // counts in the same order
compatible :: [FilePattern] -> Bool
compatible [] = True
compatible (x:xs) = all ((==) (specials x) . specials) xs
