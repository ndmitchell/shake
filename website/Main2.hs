{-# LANGUAGE RecordWildCards #-}

module Main2(main) where

import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import Text.HTML.TagSoup
import Text.Markdown
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text
import Control.Applicative
import Data.Traversable
import Control.Arrow
import Data.Functor.Identity
import Data.List
import Text.HTML.TagSoup


main :: IO ()
main = do
    createDirectoryIfMissing True "output"
    files <- getDirectoryContents "../docs"
    skeleton <- skeleton "parts" "output/index.css"
    forM_ files $ \file -> do
        when (takeExtension file == ".md") $ do
            putChar '.'
            p <- readPage $ "../docs" </> file
            skeleton ("output" </> map toLower (takeBaseName file) <.> "html") p 
    copyFile "../docs/shake-progress.png" "output/shake-progress.png"
    putStrLn " done"


data Link = Link
    {linkLevel :: String
    ,linkTitle :: String
    ,linkKey :: String
    } deriving Show

data Page = Page
    {pageTitle :: String
    ,pageTOC :: [Link]
    ,pageBody :: [Tag String]
    } deriving Show

readFileMarkdown :: FilePath -> IO [Tag String]
readFileMarkdown = fmap (parseTags . T.unpack . renderHtml . markdown def) . T.readFile

readFileTags :: FilePath -> IO [Tag String]
readFileTags = fmap parseTags . readFile 

writeFileTags :: FilePath -> [Tag String] -> IO ()
writeFileTags file = writeFile file . renderTags


---------------------------------------------------------------------
-- READ A PAGE

readPage :: FilePath -> IO Page
readPage file = do
    (pageTOC, pageBody) <- fmap links $ readFileMarkdown $ "../docs" </> file
    let pageTitle = innerText $ inside "h1" pageBody
    return Page{..}
    where
        links (TagOpen linkLevel@['h',i] at:xs) | i `elem` "123" =
                first (Link{..}:) $ second (TagOpen linkLevel (("id",linkKey):at):) $ links xs
            where linkTitle = innerText $ takeWhile (/= TagClose linkLevel) xs
                  linkKey = map toLower $ filter isAlpha linkTitle
        links (x:xs) = second (x:) $ links xs
        links [] = ([], [])


---------------------------------------------------------------------
-- POPULATE A SKELETON

skeleton :: FilePath -> FilePath -> IO (FilePath -> Page -> IO ())
skeleton dir cssOut = do
    common <- readFile $ dir </> "index.css"
    header <- readFileTags $ dir </> "header.html"
    content <- readFileTags $ dir </> "content.html"
    footer <- readFileTags $ dir </> "footer.html"
    writeFile cssOut $ common ++ style header ++ style content ++ style footer
    return $ \file Page{..} -> writeFileTags file $
        takeWhile (~/= "<div id=content>") (map (activate $ takeFileName file) $ noStyle header) ++
        parseTags "<div id=content>" ++ {- <div id=toc>" ++
        concat [ [TagOpen "a" [("class",linkLevel),("href",'#':linkKey)], TagText linkTitle, TagClose "a"]
               | Link{..} <- pageTOC] ++
        parseTags "</div>" ++ -}
        pageBody ++
        parseTags "</div>" ++
        dropWhile (~/= "<p id=footer>") footer
    where
        style = innerText . inside "style"
        noStyle x = a ++ drop 1 (dropWhile (~/= "</style>") b)
            where (a,b) = break (~== "<style>") x

        activate url (TagOpen "a" ats) = TagOpen "a" $ let act = ("class","active") in
            [act | ("href",url) `elem` ats] ++ delete act ats
        activate url x = x


inside :: String -> [Tag String] -> [Tag String]
inside tag = takeWhile (~/= TagClose tag) . dropWhile (~/= TagOpen tag [])

