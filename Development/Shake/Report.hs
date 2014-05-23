{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Development.Shake.Report(ReportEntry(..), ReportTrace(..), buildReport) where

import General.Base
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.FilePath
import Paths_shake
import qualified Data.ByteString.Lazy.Char8 as LBS


data ReportEntry = ReportEntry
    {repName :: String, repBuilt :: Int, repChanged :: Int, repDepends :: [Int], repExecution :: Double, repTraces :: [ReportTrace]}
data ReportTrace = ReportTrace
    {repCommand :: String, repStart :: Double, repStop :: Double}
repTime ReportTrace{..} = repStop - repStart


-- | Generates an report given some build system profiling data.
buildReport :: [ReportEntry] -> FilePath -> IO ()
buildReport reports out
    | takeExtension out == ".js" = writeFile out $ "var shake = \n" ++ showJSON reports
    | takeExtension out == ".json" = writeFile out $ showJSON reports
    | out == "-" = putStr $ unlines $ reportSummary reports
    | otherwise = do
        htmlDir <- getDataFileName "html"
        report <- LBS.readFile $ htmlDir </> "report.html"
        let f name | name == "data.js" = return $ LBS.pack $ "var shake = \n" ++ showJSON reports
                   | otherwise = LBS.readFile $ htmlDir </> name
        LBS.writeFile out =<< runTemplate f report


reportSummary :: [ReportEntry] -> [String]
reportSummary xs =
    ["* This database has tracked " ++ show (maximum (0 : map repChanged xs) + 1) ++ " runs."
    ,let f = show . length in "There are " ++ f xs ++ " rules (" ++ f ls ++ " rebuilt in the last run)."
    ,let f = show . sum . map (length . repTraces) in "* Building required " ++ f xs ++ " traced commands (" ++ f ls ++ " in the last run)."
    ,"* The total (unparallelised) build time is " ++ showTime (sum $ map repExecution xs) ++
        " of which " ++ showTime (sum $ map repTime $ concatMap repTraces xs) ++ " is traced commands."
    ,let f = (\(a,b) -> showTime a ++ " (" ++ b ++ ")") . maximumBy (compare `on` fst) in
        "* The longest rule takes " ++ f (map (repExecution &&& repName) xs) ++
        ", and the longest traced command takes " ++ f (map (repTime &&& repCommand) $ concatMap repTraces xs) ++ "."
    ,let sumLast = sum $ map repTime $ concatMap repTraces ls
         maxStop = maximum $ 0 : map repStop (concatMap repTraces ls) in
        "* Last run gave an average parallelism of " ++ showDP 2 (if maxStop == 0 then 0 else sumLast / maxStop) ++
        " times over " ++ showTime(maxStop) ++ "."
    ]
    where ls = filter ((==) 0 . repBuilt) xs


showJSON :: [ReportEntry] -> String
showJSON xs = "[" ++ intercalate "\n," (map showEntry xs) ++ "\n]"
    where
        showEntry ReportEntry{..} = (\xs -> "{" ++ intercalate ", " xs ++ "}") $
            ["\"name\":" ++ show repName
            ,"\"built\":" ++ show repBuilt
            ,"\"changed\":" ++ show repChanged
            ,"\"depends\":" ++ show repDepends
            ,"\"execution\":" ++ show repExecution] ++
            ["\"traces\":[" ++ intercalate "," (map showTrace repTraces) ++ "]" | not $ null repTraces]
        showTrace ReportTrace{..} =
            "{\"command\":" ++ show repCommand ++ ",\"start\":" ++ show repStart ++ ",\"stop\":" ++ show repStop ++ "}"


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
