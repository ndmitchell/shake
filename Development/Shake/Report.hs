{-# LANGUAGE PatternGuards #-}

module Development.Shake.Report(buildReport) where

import Control.Monad
import Data.Char
import Data.List
import System.FilePath
import Paths_shake


-- | Generates an HTML report given some build system
--   profiling data in JSON format.
buildReport :: String -> FilePath -> IO ()
buildReport json out = do
    htmlDir <- getDataFileName "html"
    report <- readFile $ htmlDir </> "report.html"
    let f name | name == "data.js" = return $ "var shake = \n" ++ json
               | otherwise = readFile $ htmlDir </> name
    writeFile out =<< runTemplate f report


-- | Template Engine. Perform the following replacements on a line basis:
--
-- * <script src="foo"></script> ==> <script>[[foo]]</script>
--
-- * <link href="foo" rel="stylesheet" type="text/css" /> ==> <style type="text/css">[[foo]]</style>
runTemplate :: Monad m => (String -> m String) -> String -> m String
runTemplate ask = liftM unlines . mapM f . lines
    where
        f x | Just file <- stripPrefix "<script src=\"" y = do res <- grab file; return $ "<script>\n" ++ res ++ "\n</script>"
            | Just file <- stripPrefix "<link href=\"" y = do res <- grab file; return $ "<style type=\"text/css\">\n" ++ res ++ "\n</style>"
            | otherwise = return x
            where
                y = dropWhile isSpace x
                grab = ask . takeWhile (/= '\"')
