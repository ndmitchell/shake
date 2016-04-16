{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Development.Shake.FilePattern(
    -- * Primitive API, as exposed
    FilePattern, (?==), (<//>),
    -- * Optimisation opportunities
    simple,
    -- * Multipattern file rules
    compatible, extract, substitute,
    -- * Accelerated searching
    Walk(..), walk,
    -- * Testing only
    internalTest
    ) where

import Development.Shake.Errors
import System.FilePath(isPathSeparator)
import Data.List.Extra
import Control.Applicative
import Control.Monad
import Data.Tuple.Extra
import Data.Maybe
import Prelude


-- | A type synonym for file patterns, containing @\/\/@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'normaliseEx'd 'FilePath' values are suitable as 'FilePattern' values which match
--   only that specific file. On Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators 'Development.Shake.FilePath.<.>', 'Development.Shake.FilePath.</>'
--   and 'Development.Shake.<//>'. However, beware that:
--
-- * On Windows, use 'Development.Shake.FilePath.<.>' from "Development.Shake.FilePath" instead of from
--   "System.FilePath" - otherwise @\"\/\/*\" \<.\> exe@ results in @\"\/\/*\\\\.exe\"@.
--
-- * If the second argument of 'Development.Shake.FilePath.</>' has a leading path separator (namely @\/@)
--   then the second argument will be returned.
type FilePattern = String

infixr 5 <//>

-- | Join two 'FilePattern' values by inserting two @\/@ characters between them.
--   Will first remove any trailing path separators on the first argument, and any leading
--   separators on the second.
--
-- > "dir" <//> "*" == "dir//*"
(<//>) :: FilePattern -> FilePattern -> FilePattern
a <//> b = dropWhileEnd isPathSeparator a ++ "//" ++ dropWhile isPathSeparator b


---------------------------------------------------------------------
-- PATTERNS

data Pat = Lit String -- ^ foo
         | Star   -- ^ /*/
         | Skip -- ^ //
         | Skip1 -- ^ //, but must be at least 1 element
         | Rel -- ^ Does not match absolute path (** = Rel, Skip)
         | Stars String [String] String -- ^ *foo*, prefix (fixed), infix floaters, suffix
                                        -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)

isLit Lit{} = True; isLit _ = False
fromLit (Lit x) = x


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
        f str slash (Str "**":xs) = Rel : Skip : f True False xs
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
    "x" # [Lit "x"]
    "/" # [Lit "",Lit ""]
    "x/" # [Lit "x",Lit ""]
    "/x" # [Lit "",Lit "x"]
    "x/y" # [Lit "x",Lit "y"]
    "//" # [Skip]
    "**" # [Rel, Skip]
    "//x" # [Skip, Lit "x"]
    "**/x" # [Rel, Skip, Lit "x"]
    "x//" # [Lit "x", Skip]
    "x/**" # [Lit "x", Rel, Skip]
    "x//y" # [Lit "x",Skip, Lit "y"]
    "x/**/y" # [Lit "x",Rel, Skip, Lit "y"]
    "///" # [Skip1, Lit ""]
    "**/**" # [Rel, Skip,Rel, Skip]
    "**/**/" # [Rel, Skip, Rel, Skip, Lit ""]
    "///x" # [Skip1, Lit "x"]
    "**/x" # [Rel, Skip, Lit "x"]
    "x///" # [Lit "x", Skip, Lit ""]
    "x/**/" # [Lit "x", Rel, Skip, Lit ""]
    "x///y" # [Lit "x",Skip, Lit "y"]
    "x/**/y" # [Lit "x",Rel, Skip, Lit "y"]
    "////" # [Skip, Skip]
    "**/**/**" # [Rel, Skip, Rel, Skip, Rel, Skip]
    "////x" # [Skip, Skip, Lit "x"]
    "x////" # [Lit "x", Skip, Skip]
    "x////y" # [Lit "x",Skip, Skip, Lit "y"]
    "**//x" # [Rel, Skip, Skip, Lit "x"]


-- | Optimisations that may change the matched expressions
optimise :: [Pat] -> [Pat]
optimise (Skip:Skip:xs) = optimise $ Skip:xs
optimise (Skip:Star:xs) = optimise $ Skip1:xs
optimise (Star:Skip:xs) = optimise $ Skip1:xs
optimise (x:xs) = x : optimise xs
optimise [] =[]

-- | Given a pattern, and a list of path components, return a
-- list of all matches (for each wildcard in order, what the
-- wildcard matched.)
match :: [Pat] -> [String] -> [[String]]
match (Skip:xs) (y:ys) = map ("":) (match xs (y:ys)) ++ match (Skip1:xs) (y:ys)
match (Skip1:xs) (y:ys) = [(y++"/"++r):rs | r:rs <- match (Skip:xs) ys]
match (Skip:xs) [] = map ("":) $ match xs []
match (Rel:xs) ("":_) = []
match (Rel:xs) ys = match xs ys
match (Star:xs) (y:ys) = map (y:) $ match xs ys
match (Lit x:xs) (y:ys) | x == y = match xs ys
match (x@Stars{}:xs) (y:ys) | Just rs <- matchStars x y = map (rs ++) $ match xs ys
match [] [] = [[]]
match _ _ = []


matchOne :: Pat -> String -> Bool
matchOne (Lit x) y = x == y
matchOne x@Stars{} y = isJust $ matchStars x y
matchOne Star _ = True


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


--- | Match a 'FilePattern' against a 'FilePath', There are three special forms:
---
--- * @*@ matches an entire path component, excluding any separators.
---
--- * @\/\/@ matches an arbitrary number of path components.  At the
--    beginning of a path, this matches absolute paths.
--
--  * @**@ as a path component matches an arbitrary number of path components,
--    and, when a prefix, does NOT match absolute paths.
--    Currently considered experimental.
---
---   Some examples:
---
--- * @test.c@ matches @test.c@ and nothing else.
---
--- * @*.c@ matches all @.c@ files in the current directory, so @file.c@ matches,
---   but @file.h@ and @dir\/file.c@ don't.
---
--- * @\/\/*.c@ matches all @.c@ files anywhere on the filesystem,
---   so @file.c@, @dir\/file.c@, @dir1\/dir2\/file.c@ and @/path/to/file.c@ all match, but @file.h@ and
---   @dir\/file.h@ don't.
---
--- * @**.c@ matches all @.c@ files in the current directory and
--    directories below it so @file.c@, @dir\/file.c@ and
--    @dir1\/dir2\/file.c@ match, but @/path/to/file.c@ does not.
---
--- * @dir\/*\/*@ matches all files one level below @dir@, so @dir\/one\/file.c@ and
---   @dir\/two\/file.h@ match, but @file.c@, @one\/dir\/file.c@, @dir\/file.h@
---   and @dir\/one\/two\/file.c@ don't.
---
---   Patterns with constructs such as @foo\/..\/bar@ will never match
---   normalised 'FilePath' values, so are unlikely to be correct.
(?==) :: FilePattern -> FilePath -> Bool
(?==) p = case optimise $ parse p of
    [Skip] -> const True
    [Skip1] -> const True
    p -> not . null . match p . split isPathSeparator


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

specials :: FilePattern -> [Pat]
specials = concatMap f . parse
    where
        f Lit{} = []
        f Star = [Star]
        f Skip = [Skip]
        f Skip1 = [Skip]
        f Rel = []
        f (Stars _ xs _) = replicate (length xs + 1) Star

-- | Is the pattern free from any * and //.
simple :: FilePattern -> Bool
simple = null . specials

-- | Do they have the same * and // counts in the same order
compatible :: [FilePattern] -> Bool
compatible [] = True
compatible (x:xs) = all ((==) (specials x) . specials) xs

-- | Extract the items that match the wildcards. The pair must match with '?=='.
extract :: FilePattern -> FilePath -> [String]
extract p = let pat = parse p in \x ->
    case match pat (split isPathSeparator x) of
        [] | p ?== x -> err $ "extract with " ++ show p ++ " and " ++ show x
           | otherwise -> error $ "Pattern " ++ show p ++ " does not match " ++ x ++ ", when trying to extract the FilePattern matches"
        ms:_ -> ms


-- | Given the result of 'extract', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substitute :: [String] -> FilePattern -> FilePath
substitute oms oxs = intercalate "/" $ concat $ snd $ mapAccumL f oms (parse oxs)
    where
        f ms (Lit x) = (ms, [x])
        f (m:ms) Star = (ms, [m])
        f (m:ms) Skip = (ms, split m)
        f (m:ms) Skip1 = (ms, split m)
        f ms Rel = (ms, [])
        f ms (Stars pre mid post) = (ms2, [concat $ pre : zipWith (++) ms1 (mid++[post])])
            where (ms1,ms2) = splitAt (length mid + 1) ms
        f _ _ = error $ "Substitution failed into pattern " ++ show oxs ++ " with " ++ show (length oms) ++ " matches, namely " ++ show oms

        split = linesBy (== '/')


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Given a list of files, return a list of things I can match in this directory
--   plus a list of subdirectories and walks that apply to them.
--   Use WalkTo when the list can be predicted in advance
data Walk = Walk ([String] -> ([String],[(String,Walk)]))
          | WalkTo            ([String],[(String,Walk)])

walk :: [FilePattern] -> (Bool, Walk)
walk ps = let ps2 = map (optimise . parse) ps in (any (\p -> isEmpty p || not (null $ match p [""])) ps2, f ps2)
    where
        f (nubOrd -> ps)
            | all isLit fin, all (isLit . fst) nxt = WalkTo (map fromLit fin, map (fromLit *** f) nxt)
            | otherwise = Walk $ \xs ->
                (if finStar then xs else filter (\x -> any (`matchOne` x) fin) xs
                ,[(x, f ys) | x <- xs, let ys = concat [b | (a,b) <- nxt, matchOne a x], not $ null ys])
            where
                finStar = Star `elem` fin
                fin = nubOrd $ mapMaybe final ps
                nxt = groupSort $ concatMap next ps


next :: [Pat] -> [(Pat, [Pat])]
next (Skip1:xs) = [(Star,Skip:xs)]
next (Skip:xs) = (Star,Skip:xs) : next xs
next (Rel:xs) = next xs
next (x:xs) = [(x,xs) | not $ null xs]
next [] = []

final :: [Pat] -> Maybe Pat
final (Skip:xs) = if isEmpty xs then Just Star else final xs
final (Skip1:xs) = if isEmpty xs then Just Star else Nothing
final (Rel:xs) = final xs
final (x:xs) = if isEmpty xs then Just x else Nothing
final [] = Nothing

isEmpty = all (== Skip)
