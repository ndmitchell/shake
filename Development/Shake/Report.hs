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
buildReport :: FilePath -> [ReportEntry] -> IO ()
buildReport out xs
    | takeExtension out == ".js" = writeFile out $ "var shake = \n" ++ reportJSON xs
    | takeExtension out == ".json" = writeFile out $ reportJSON xs
    | takeExtension out == ".trace" = writeFile out $ reportTrace xs
    | out == "-" = putStr $ unlines $ reportSummary xs
    | otherwise = LBS.writeFile out =<< reportHTML xs


reportSummary :: [ReportEntry] -> [String]
reportSummary xs =
    ["* This database has tracked " ++ show (maximum (0 : map repChanged xs) + 1) ++ " runs."
    ,let f = show . length in "* There are " ++ f xs ++ " rules (" ++ f ls ++ " rebuilt in the last run)."
    ,let f = show . sum . map (length . repTraces) in "* Building required " ++ f xs ++ " traced commands (" ++ f ls ++ " in the last run)."
    ,"* The total (unparallelised) build time is " ++ showTime (sum $ map repExecution xs) ++
        " of which " ++ showTime (sum $ map repTime $ concatMap repTraces xs) ++ " is traced commands."
    ,let f xs = if null xs then "0s" else (\(a,b) -> showTime a ++ " (" ++ b ++ ")") $ maximumBy (compare `on` fst) xs in
        "* The longest rule takes " ++ f (map (repExecution &&& repName) xs) ++
        ", and the longest traced command takes " ++ f (map (repTime &&& repCommand) $ concatMap repTraces xs) ++ "."
    ,let sumLast = sum $ map repTime $ concatMap repTraces ls
         maxStop = maximum $ 0 : map repStop (concatMap repTraces ls) in
        "* Last run gave an average parallelism of " ++ showDP 2 (if maxStop == 0 then 0 else sumLast / maxStop) ++
        " times over " ++ showTime(maxStop) ++ "."
    ]
    where ls = filter ((==) 0 . repBuilt) xs


reportHTML :: [ReportEntry] -> IO LBS.ByteString
reportHTML xs = do
    htmlDir <- getDataFileName "html"
    report <- LBS.readFile $ htmlDir </> "report.html"
    let f name | name == "data.js" = return $ LBS.pack $ "var shake = \n" ++ reportJSON xs
               | otherwise = LBS.readFile $ htmlDir </> name
    runTemplate f report


reportTrace :: [ReportEntry] -> String
reportTrace xs = jsonListLines $
    showEntries 0 [y{repCommand=repName x} | x <- xs, y <- repTraces x] ++
    showEntries 1 (concatMap repTraces xs)
    where
        showEntries pid xs = map (showEntry pid) $ snd $ mapAccumL alloc [] $ sortBy (compare `on` repStart) xs
        alloc as r | (a1,an:a2) <- break (\a -> repStop a <= repStart r) as = (a1++r:a2, (length a1,r))
                   | otherwise = (as++[r], (length as,r))
        showEntry pid (tid, ReportTrace{..}) = jsonObject
            [("args","{}"), ("ph",show "X"), ("cat",show "target")
            ,("name",show repCommand), ("tid",show tid), ("pid",show pid)
            ,("ts",show $ 1000000*repStart), ("dur",show $ 1000000*(repStop-repStart))]


reportJSON :: [ReportEntry] -> String
reportJSON = jsonListLines . map showEntry
    where
        showEntry ReportEntry{..} = jsonObject $
            [("name", show repName)
            ,("built", show repBuilt)
            ,("changed", show repChanged)
            ,("depends", show repDepends)
            ,("execution", show repExecution)] ++
            [("traces", jsonList $ map showTrace repTraces) | not $ null repTraces]
        showTrace ReportTrace{..} = jsonObject
            [("command",show repCommand), ("start",show repStart), ("stop",show repStop)]

jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
jsonObject xs = "{" ++ intercalate ", " [show a ++ ":" ++ b | (a,b) <- xs] ++ "}"

---------------------------------------------------------------------
-- TEMPLATE ENGINE

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
