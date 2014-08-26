{-# LANGUAGE Rank2Types, ViewPatterns #-}

module Library(module Library) where

import Text.Markdown
import qualified Data.Text.Lazy as T
import Text.Blaze.Html.Renderer.Text
import Control.Applicative
import Data.Traversable
import Control.Arrow
import Data.Functor.Identity
import Data.List
import Text.HTML.TagSoup


---------------------------------------------------------------------
-- MARKDOWN

convertMarkdown :: String -> String
convertMarkdown = renderTags . f . parseTags . T.unpack . renderHtml . markdown def . T.pack
    where
        f (TagOpen "pre" []:TagClose "pre":xs) = f $ TagClose "pre":xs
            -- BUG: inserts random extra pre tag
        f (TagComment x:xs) = f $ parseTags x ++ xs
        f (x:xs) = x : f xs
        f [] = []


---------------------------------------------------------------------
-- TAG SOUP

type Tags = [Tag String]
type Attribs = [Attribute String]

tagOpen :: String -> Attribs -> Tags -> Tags
tagOpen a b c = TagOpen a b : c ++ [TagClose a]

tag :: TagRep t => t -> Tags -> Tags 
tag (toTagRep -> TagOpen a b) c = tagOpen a b c

inner :: Tags -> Tags
inner (TagOpen x _:xs) | [TagClose x] `isSuffixOf` xs = init xs
inner x = error "inner mismatch"


---------------------------------------------------------------------
-- LENS

type Lens a b = forall f . Applicative f => (b -> f b) -> (a -> f a)

infixr 4 `self`, `literal`, `whole`

self :: Lens a b -> (b -> b) -> a -> a
self l f = runIdentity . l (Identity .  f)

literal :: Lens a b -> b -> a -> a
literal l v = self l $ const v

whole :: Lens a b -> (a -> b) -> a -> a
whole l f x = self l (const $ f x) x

getL :: Lens a b -> a -> [b]
getL l x = getConst (l (Const . return) x)

getLL :: Lens a [b] -> a -> [b]
getLL l = concat . getL l


---------------------------------------------------------------------
-- TAG SOUP LENS

matchLens :: Lens [a] [(Bool, [a])]
matchLens f x = undefined

tagPred :: (String -> Bool) -> Lens Tags Tags
tagPred pred apply x = concat <$> sequenceA [if b then apply x else pure x | (b,x) <- f x]
    where
        f :: Tags -> [(Bool, Tags)]
        f (TagOpen name at:xs) | pred name, (xs,ys) <- g name 0 xs = (True,TagOpen name at:xs) : f ys
        f (x:xs) = (False,[x]) : f xs
        f [] = []

        g :: String -> Int -> Tags -> (Tags, Tags)
        g name i (TagClose x:xs) | x == name = first (TagClose x:) $ if i == 0 then ([], xs) else g name (i-1) xs
        g name i (TagOpen x a:xs) | x == name = first (TagOpen x a :) $ g name (i+1) xs
        g name i (x:xs) = first (x:) $ g name i xs
        g name i [] = error $ "Matching tag " ++ name ++ " not found"


tagName :: String -> Lens Tags Tags
tagName x = tagPred (== x)

heading :: Lens Tags Tags
heading = tagPred $ \x -> case x of
    ['h',i] | i `elem` "123456" -> True
    _ -> False

innerL :: Lens Tags Tags
innerL f (TagOpen x a:xs) | [TagClose x] `isSuffixOf` xs = tagOpen x a <$> f (init xs)
innerL f x = error $ "innerL mismatch: " ++ renderTags x
