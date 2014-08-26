{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Development.Shake.Profile(ProfileEntry(..), ProfileTrace(..), buildReport) where

import General.Base
import General.Template
import Control.Arrow
import Data.Function
import Data.List
import Data.Version
import System.FilePath
import Paths_shake
import qualified Data.ByteString.Lazy.Char8 as LBS


data ProfileEntry = ProfileEntry
    {repName :: String, repBuilt :: Int, repChanged :: Int, repDepends :: [Int], repExecution :: Double, repTraces :: [ProfileTrace]}
data ProfileTrace = ProfileTrace
    {repCommand :: String, repStart :: Double, repStop :: Double}
repTime ProfileTrace{..} = repStop - repStart


-- | Generates an report given some build system profiling data.
buildReport :: FilePath -> [ProfileEntry] -> IO ()
buildReport out xs
    | takeExtension out == ".js" = writeFile out $ "var shake = \n" ++ reportJSON xs
    | takeExtension out == ".json" = writeFile out $ reportJSON xs
    | takeExtension out == ".trace" = writeFile out $ reportTrace xs
    | out == "-" = putStr $ unlines $ reportSummary xs
    | otherwise = LBS.writeFile out =<< reportHTML xs


reportSummary :: [ProfileEntry] -> [String]
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


reportHTML :: [ProfileEntry] -> IO LBS.ByteString
reportHTML xs = do
    htmlDir <- getDataFileName "html"
    report <- LBS.readFile $ htmlDir </> "report.html"
    let f name | name == "data.js" = return $ LBS.pack $ "var shake = \n" ++ reportJSON xs
               | name == "version.js" = return $ LBS.pack $ "var version = " ++ show (showVersion version)
               | otherwise = LBS.readFile $ htmlDir </> name
    runTemplate f report


reportTrace :: [ProfileEntry] -> String
reportTrace xs = jsonListLines $
    showEntries 0 [y{repCommand=repName x} | x <- xs, y <- repTraces x] ++
    showEntries 1 (concatMap repTraces xs)
    where
        showEntries pid xs = map (showEntry pid) $ snd $ mapAccumL alloc [] $ sortBy (compare `on` repStart) xs
        alloc as r | (a1,an:a2) <- break (\a -> repStop a <= repStart r) as = (a1++r:a2, (length a1,r))
                   | otherwise = (as++[r], (length as,r))
        showEntry pid (tid, ProfileTrace{..}) = jsonObject
            [("args","{}"), ("ph",show "X"), ("cat",show "target")
            ,("name",show repCommand), ("tid",show tid), ("pid",show pid)
            ,("ts",show $ 1000000*repStart), ("dur",show $ 1000000*(repStop-repStart))]


reportJSON :: [ProfileEntry] -> String
reportJSON = jsonListLines . map showEntry
    where
        showEntry ProfileEntry{..} = jsonObject $
            [("name", show repName)
            ,("built", show repBuilt)
            ,("changed", show repChanged)
            ,("depends", show repDepends)
            ,("execution", show repExecution)] ++
            [("traces", jsonList $ map showTrace repTraces) | not $ null repTraces]
        showTrace ProfileTrace{..} = jsonObject
            [("command",show repCommand), ("start",show repStart), ("stop",show repStop)]

jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
jsonObject xs = "{" ++ intercalate ", " [show a ++ ":" ++ b | (a,b) <- xs] ++ "}"
