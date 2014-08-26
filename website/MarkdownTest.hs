
module MarkdownTest(main) where

import Text.Markdown
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text


main :: IO ()
main = do
    let s = T.pack "<pre>\na\n\nb\n</pre>"
    T.putStrLn s
    putStrLn "msXssProtect=True"
    T.putStrLn $ renderHtml $ markdown def{msXssProtect=True} s
    putStrLn "msXssProtect=False"
    T.putStrLn $ renderHtml $ markdown def{msXssProtect=False} s
