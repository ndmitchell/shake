{-# LANGUAGE ViewPatterns #-}

module General.Template(runTemplate) where

import System.FilePath.Posix
import Control.Exception.Extra
import Control.Monad.IO.Class
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Language.Javascript.Flot as Flot
import qualified Language.Javascript.JQuery as JQuery


libraries =
    [("jquery.js", JQuery.file)
    ,("jquery.flot.js", Flot.file Flot.Flot)
    ,("jquery.flot.stack.js", Flot.file Flot.FlotStack)
    ]

-- | Template Engine. Perform the following replacements on a line basis:
--
-- * <script src="foo"></script> ==> <script>[[foo]]</script>
--
-- * <link href="foo" rel="stylesheet" type="text/css" /> ==> <style type="text/css">[[foo]]</style>
runTemplate :: MonadIO m => (FilePath -> m LBS.ByteString) -> LBS.ByteString -> m LBS.ByteString
runTemplate ask = fmap LBS.unlines . mapM f . LBS.lines
    where
        link = LBS.pack "<link href=\""
        script = LBS.pack "<script src=\""

        f x | Just file <- lbs_stripPrefix script y = do res <- grab file; return $ LBS.pack "<script>\n" `LBS.append` res `LBS.append` LBS.pack "\n</script>"
            | Just file <- lbs_stripPrefix link y = do res <- grab file; return $ LBS.pack "<style type=\"text/css\">\n" `LBS.append` res `LBS.append` LBS.pack "\n</style>"
            | otherwise = return x
            where
                y = LBS.dropWhile isSpace x
                grab = asker . takeWhile (/= '\"') . LBS.unpack

        asker o@(splitFileName -> ("lib/",x)) = case lookup x libraries of
            Just act -> liftIO $ LBS.readFile =<< act
            Nothing -> liftIO $ errorIO $ "Template library, unknown library: " ++ o
        asker x = ask x


lbs_stripPrefix :: LBS.ByteString -> LBS.ByteString -> Maybe LBS.ByteString
lbs_stripPrefix prefix text = if a == prefix then Just b else Nothing
    where (a,b) = LBS.splitAt (LBS.length prefix) text
