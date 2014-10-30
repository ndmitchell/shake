{-# LANGUAGE ViewPatterns #-}

module Code(code) where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.HTML.TagSoup
import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import System.Directory
import System.FilePath


code :: String -> [Tag String]
code x | x == "\"" || "#!" `isPrefixOf` x || "3m" `isPrefixOf` x = [TagText x]
code xs = concatMap f $ lexer xs
    where
        f x | x `elem` ["import","do","let"] = spn "key" x
        f [x] | x `elem` "(){}[]\\=|" = spn "sym" [x]
        f x | x `elem` ["->","::","<-"] = spn "sym" x
        f (x:xs) | x `elem` "\"\'" = spn "str" $ x:xs
        f x = [TagText x]

spn cls x = [TagOpen "span" [("class",cls)], TagText x, TagClose "span"]


lexer :: String -> [String]
lexer [] = []
lexer x@(c:_) | isSpace c = let (a,b) = span isSpace x in a : lexer b
lexer (lex -> [(a,b)]) = a : lexer b
lexer (x:xs) = [x] : lexer xs
