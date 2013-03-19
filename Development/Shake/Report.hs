{-# LANGUAGE PatternGuards #-}

module Development.Shake.Report(buildReport) where

import Control.Monad
import Data.Char
import System.FilePath
import Paths_shake
import qualified Data.ByteString.Lazy.Char8 as LBS


-- | Generates an HTML report given some build system
--   profiling data in JSON format.
buildReport :: String -> FilePath -> IO ()
buildReport json out = do
    htmlDir <- getDataFileName "html"
    report <- LBS.readFile $ htmlDir </> "report.html"
    let f name | name == "data.js" = return $ LBS.pack $ "var shake = \n" ++ json
               | otherwise = LBS.readFile $ htmlDir </> name
    LBS.writeFile out =<< runTemplate f report


-- | Template Engine. Perform the following replacements on a line basis:
--
-- * <script src="foo"></script> ==> <script>[[foo]]</script>
--
-- * <link href="foo" rel="stylesheet" type="text/css" /> ==> <style type="text/css">[[foo]]</style>
runTemplate :: Monad m => (FilePath -> m LBS.ByteString) -> LBS.ByteString -> m LBS.ByteString
runTemplate ask = liftM LBS.unlines . mapM f . LBS.lines
    where
        link = LBS.pack "<link href=\""
        script = LBS.pack "<script src=\""

        f x | Just file <- lbs_stripPrefix script y = do res <- grab file; return $ LBS.pack "<script>\n" `LBS.append` res `LBS.append` LBS.pack "\n</script>"
            | Just file <- lbs_stripPrefix link y = do res <- grab file; return $ LBS.pack "<style type=\"text/css\">\n" `LBS.append` res `LBS.append` LBS.pack "\n</style>"
            | otherwise = return x
            where
                y = LBS.dropWhile isSpace x
                grab = ask . takeWhile (/= '\"') . LBS.unpack


lbs_stripPrefix :: LBS.ByteString -> LBS.ByteString -> Maybe LBS.ByteString
lbs_stripPrefix prefix text = if a == prefix then Just b else Nothing
    where (a,b) = LBS.splitAt (LBS.length prefix) text
