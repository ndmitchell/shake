
module Main(main) where

import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe
import Library
import Data.Char
import Data.List
import Text.HTML.TagSoup


main :: IO ()
main = do
    createDirectoryIfMissing True "output"
    files <- getDirectoryContents "../docs"
    prefix <- readFile "prefix.txt"
    suffix <- readFile "suffix.txt"
    forM_ files $ \file -> do
        when (takeExtension file == ".md") $ do
            putChar '.'
            src <- readFile $ "../docs" </> file
            let html = parseTags $ prefix ++ convertMarkdown src ++ suffix
            let dir = if takeBaseName file == "Index" then "output" else "output" </> map toLower (takeBaseName file)
            createDirectoryIfMissing True dir
            writeFile (dir </> "index.html") $ renderTags (operations html)
    copyFile "index.css" "output/index.css"
    copyFile "../docs/shake-progress.png" "output/shake-progress.png"
    putStrLn " done"

operations :: Tags -> Tags
operations xs =
    tagName "neil:body" `self` inner $
    tagName "neil:toc" `whole` tableOfContents $
    heading `self` addId $
    tagName "a" `self` markdownLink $
    tagName "neil:body" . tagName "h1" `literal` [] $
    tagName "neil:h1" `whole` getLL (tagName "neil:body" . tagName "h1" . innerL) $
    tagName "neil:home" `self` tag "<div id=messages>" . inner $
    tagName "neil:home" . tagName "h2" `self` (parseTags "<br class=gap />" ++) $
    xs


tableOfContents :: Tags -> Tags
tableOfContents xs = tag "<ul>" $ concat
    [ tag "<li>" $ tagOpen "a" [("href",'#' : fromJust (lookup "id" a))] $ inner o
    | o@(TagOpen h a:_) <- getL heading xs, h /= "h1"]

addId :: Tags -> Tags
addId (TagOpen x a:xs) = TagOpen x (("id", norm $ innerText xs):a) : xs
    where norm = intercalate "-" . words . filter (\x -> x == ' ' || isAlpha x) . map toLower

markdownLink :: Tags -> Tags
markdownLink (TagOpen "a" a:xs) = TagOpen "a" (map f a) : xs
    where f ("href",x) | takeExtension x == ".md" = ("href", map toLower (takeBaseName x) ++ "/index.html")
          f x = x
markdownLink x = x
