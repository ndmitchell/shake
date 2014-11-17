{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import Data.Tuple.Extra
import Control.Monad
import Data.Char
import Data.List.Extra
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.HTML.TagSoup
import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Extra
import Code

data Mode = Debug | Release deriving Eq

main :: IO ()
main = do
    args <- getArgs
    let mode = if null args then Release else Debug
    createDirectoryIfMissing True "output"
    files <- getDirectoryContents "../docs"
    code <- code "../dist/doc/html/shake/shake.txt"
    skeleton <- skeleton mode "parts" "output/index.css"
    forM_ files $ \file -> do
        when (takeExtension file == ".md") $ do
            putChar '.'
            p <- readPage mode code $ "../docs" </> file
            skeleton ("output" </> map toLower (takeBaseName file) <.> "html") p 
    copyFile "../docs/shake-progress.png" "output/shake-progress.png"
    copyFile "parts/favicon.ico" "output/favicon.ico"
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
readFileMarkdown = fmap (parseTags . T.unpack . renderHtml . markdown def{msXssProtect=False}) . T.readFile

readFileTags :: FilePath -> IO [Tag String]
readFileTags = fmap parseTags . readFile'

writeFileTags :: FilePath -> [Tag String] -> IO ()
writeFileTags file = writeFile file . renderTags


---------------------------------------------------------------------
-- READ A PAGE

readPage :: Mode -> (String -> [Tag String]) -> FilePath -> IO Page
readPage mode code file = do
    (pageTOC, pageBody) <- fmap (links . reformat mode code) $ readFileMarkdown $ "../docs" </> file
    let pageTitle = innerText $ inside "h1" pageBody
    return Page{..}
    where
        links (TagOpen linkLevel@['h',i] at:xs) | i `elem` "23" =
                first (Link{..}:) $ second (\xs -> TagOpen "span" [("class","target"),("id",linkKey)]:TagClose "span":TagOpen linkLevel at:xs) $ links xs
            where linkTitle = innerText $ takeWhile (/= TagClose linkLevel) xs
                  linkKey = intercalate "-" $ map (map toLower . filter isAlpha) $ words linkTitle
        links (x:xs) = second (x:) $ links xs
        links [] = ([], [])


reformat :: Mode -> (String -> [Tag String]) -> [Tag String] -> [Tag String]
reformat mode code (TagOpen "p" []:TagOpen "i" []:TagText s:xs) | "See also" `isPrefixOf` s =
    reformat mode code $ drop 1 $ dropWhile (~/= "</p>") xs
reformat mode code (TagOpen "a" at:xs) = TagOpen "a" (map f at) : reformat mode code xs
    where f ("href",x) | ".md" `isPrefixOf` takeExtension x =
                -- watch out for Manual.md#readme
                ("href", dropFileName x ++ map toLower (takeBaseName x) ++
                         (if mode == Release then "" else ".html") ++
                         drop 3 (takeExtension x))
          f x = x
reformat mode code (TagOpen "pre" []:TagOpen "code" []:xs) = reformat mode code $ TagOpen "pre" [] : xs
reformat mode code (TagClose "code":TagClose "pre":xs) = reformat mode code $ TagClose "pre" : xs
reformat mode code (TagOpen t at:xs) | t `elem` ["pre","code"] = TagOpen t at : concatMap f a ++ reformat mode code b
    where (a,b) = break (== TagClose t) xs
          f (TagText x) = code x
          f x = [x]
reformat mode code (x:xs) = x : reformat mode code xs
reformat mode code [] = []


---------------------------------------------------------------------
-- POPULATE A SKELETON

skeleton :: Mode -> FilePath -> FilePath -> IO (FilePath -> Page -> IO ())
skeleton mode dir cssOut = do
    common <- readFile' $ dir </> "index.css"
    header <- readFileTags $ dir </> "header.html"
    content <- readFileTags $ dir </> "content.html"
    footer <- readFileTags $ dir </> "footer.html"
    writeFile cssOut $ common ++ style header ++ style content ++ style footer
    return $ \file Page{..} -> writeFileTags file $
        inject (takeBaseName file) (takeWhile (~/= "<div id=content>") (remode $ map (activate $ takeFileName file) $ noStyle header)) ++
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
        remode xs = if mode == Debug then xs else map f xs
            where f (TagOpen "a" at) = TagOpen "a" $ for at $ second $ \v -> if takeExtension v == ".html" then dropExtension v else v
                  f x = x

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

