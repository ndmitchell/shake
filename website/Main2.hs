{-# LANGUAGE RecordWildCards #-}

module Main2(main) where

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
import Code


main :: IO ()
main = do
    createDirectoryIfMissing True "output"
    files <- getDirectoryContents "../docs"
    code <- code "../dist/doc/html/shake/shake.txt"
    skeleton <- skeleton "parts" "output/index.css"
    forM_ files $ \file -> do
        when (takeExtension file == ".md") $ do
            putChar '.'
            p <- readPage code $ "../docs" </> file
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

readPage :: (String -> [Tag String]) -> FilePath -> IO Page
readPage code file = do
    (pageTOC, pageBody) <- fmap (links . reformat code) $ readFileMarkdown $ "../docs" </> file
    let pageTitle = innerText $ inside "h1" pageBody
    return Page{..}
    where
        links (TagOpen linkLevel@['h',i] at:xs) | i `elem` "23" =
                first (Link{..}:) $ second (\xs -> TagOpen "span" [("class","target"),("id",linkKey)]:TagClose "span":TagOpen linkLevel at:xs) $ links xs
            where linkTitle = innerText $ takeWhile (/= TagClose linkLevel) xs
                  linkKey = intercalate "-" $ map (map toLower . filter isAlpha) $ words linkTitle
        links (x:xs) = second (x:) $ links xs
        links [] = ([], [])


reformat :: (String -> [Tag String]) -> [Tag String] -> [Tag String]
reformat code (TagOpen "p" []:TagOpen "i" []:TagText s:xs) | "See also" `isPrefixOf` s =
    reformat code $ drop 1 $ dropWhile (~/= "</p>") xs
reformat code (TagOpen "a" at:xs) = TagOpen "a" (map f at) : reformat code xs
    where f ("href",x) | ".md" `isPrefixOf` takeExtension x =
                -- watch out for Manual.md#readme
                ("href", dropFileName x ++ map toLower (takeBaseName x) <.> "html")
          f x = x
reformat code (TagOpen "pre" []:TagOpen "code" []:xs) = reformat code $ TagOpen "pre" [] : xs
reformat code (TagClose "code":TagClose "pre":xs) = reformat code $ TagClose "pre" : xs
reformat code (TagOpen t at:xs) | t `elem` ["pre","code"] = TagOpen t at : concatMap f a ++ reformat code b
    where (a,b) = break (== TagClose t) xs
          f (TagText x) = code x
          f x = [x]
reformat code (x:xs) = x : reformat code xs
reformat code [] = []


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
        inject (takeBaseName file) (takeWhile (~/= "<div id=content>") (map (activate $ takeFileName file) $ noStyle header)) ++
        parseTags "<div id=content>" ++
        (if length pageTOC <= 1 then [] else
            parseTags "<div id=toc>" ++
            concat [ [TagOpen "a" [("class",linkLevel),("href",'#':linkKey)], TagText linkTitle, TagClose "a"]
                   | Link{..} <- pageTOC] ++
            parseTags "</div>") ++
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

        inject name (TagOpen "body" at:xs) = TagOpen "body" (("class","page-"++name):at) : inject name xs
        inject name (x:xs) = x : inject name xs
        inject name [] = []


inside :: String -> [Tag String] -> [Tag String]
inside tag = takeWhile (~/= TagClose tag) . dropWhile (~/= TagOpen tag [])

