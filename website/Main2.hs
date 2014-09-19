
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
            writeFile ("output" </> takeBaseName file <.> "html") $ skeleton p
    copyFile "../docs/shake-progress.png" "output/shake-progress.png"
    putStrLn " done"


data Link = Link
    {linkLevel :: String
    ,linkTitle :: String
    ,linkKey :: String
    }

data Page = Page
    {pageTitle :: String
    ,pageTOC :: Link
    ,pageBody :: String
    }

readFileMarkdown :: FilePath -> IO [Tag String]
readFileMarkdown = fmap (parseTags . T.unpack . renderHtml . markdown def) . T.readFile

readFileTags :: FilePath -> IO [Tag String]
readFileTags = fmap parseTags . readFile 

writeFileTags :: FilePath -> [Tag String] -> IO ()
writeFileTags = undefined


---------------------------------------------------------------------
-- READ A PAGE

readPage :: FilePath -> IO Page
readPage file = do
    src <- readFileMarkdown $ "../docs" </> file
    return undefined


---------------------------------------------------------------------
-- POPULATE A SKELETON

skeleton :: FilePath -> FilePath -> IO (Page -> String)
skeleton dir cssOut = do
    common <- readFile $ dir </> "index.css"
    header <- readFileTags $ dir </> "header.html"
    content <- readFileTags $ dir </> "content.html"
    footer <- readFileTags $ dir </> "footer.html"
    writeFile cssOut $ common ++ style header ++ style content ++ style footer
    return $ \p -> undefined $
        takeWhile (~/= "<div id=content>") header ++
        dropWhile (~/= "<p id=footer>") footer
    where
        style = innerText . takeWhile (~/= "</style>") . dropWhile (~/= "<style>")
