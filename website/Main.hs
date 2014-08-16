
module Main(main) where

import Text.Markdown
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text
import System.FilePath
import System.Directory
import Control.Monad
import Data.Monoid
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
            src <- T.readFile $ "../docs" </> file
            let html = parseTags $ prefix ++ T.unpack (renderHtml $ markdown def src) ++ suffix
            writeFile ("output" </> map toLower (takeBaseName file) <.> "html") $ renderTags (fixup html)
    copyFile "index.css" "output/index.css"
    copyFile "../docs/shake-progress.png" "output/shake-progress.png"


fixup :: [Tag String] -> [Tag String]
fixup o = f [] o
    where
        f s (TagOpen "pre" []:TagClose "pre":xs) = f s $ TagClose "pre":xs
            -- BUG: inserts random extra pre tag
        f s (TagComment x:xs) = f s $ parseTags (tail $ init x) ++ xs
            -- BUG: tail/init combo

        -- special tags
        f s (TagOpen "h2" a:xs) | "insert:br" `elem` s =
            TagOpen "br" [("class","gap")] : TagClose "br" : TagOpen "h2" a : f s xs
        f s (TagOpen "h1" _:xs) | "remove:h1" `elem` s = f s $ drop 1 $ dropWhile (~/= "</h1>") xs
        f s (TagOpen x []:xs) | x `elem` ["insert:br","remove:h1"] = f (x:s) xs
        f s (TagOpen "copy:h1" []:xs) = [x | TagOpen "h1" _:x:_ <- tails xs] ++ f s xs

        f s (x:xs) = x : f s xs
        f s [] = []
