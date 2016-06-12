{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Development.Shake.Internal.Profile(ProfileEntry(..), ProfileTrace(..), writeProfile) where

import General.Template
import Data.Tuple.Extra
import Data.Function
import Data.List
import Data.Version
import System.FilePath
import Numeric.Extra
import General.Extra
import Paths_shake
import System.Time.Extra
import qualified Data.ByteString.Lazy.Char8 as LBS


data ProfileEntry = ProfileEntry
    {prfName :: String, prfBuilt :: Int, prfChanged :: Int, prfDepends :: [Int], prfExecution :: Double, prfTraces :: [ProfileTrace]}
data ProfileTrace = ProfileTrace
    {prfCommand :: String, prfStart :: Double, prfStop :: Double}
prfTime ProfileTrace{..} = prfStop - prfStart


-- | Generates an report given some build system profiling data.
writeProfile :: FilePath -> [ProfileEntry] -> IO ()
writeProfile out xs
    | takeExtension out == ".js" = writeFile out $ "var shake = \n" ++ generateJSON xs
    | takeExtension out == ".json" = writeFile out $ generateJSON xs
    | takeExtension out == ".trace" = writeFile out $ generateTrace xs
    | out == "-" = putStr $ unlines $ generateSummary xs
    | otherwise = LBS.writeFile out =<< generateHTML xs


generateSummary :: [ProfileEntry] -> [String]
generateSummary xs =
    ["* This database has tracked " ++ show (maximum (0 : map prfChanged xs) + 1) ++ " runs."
    ,let f = show . length in "* There are " ++ f xs ++ " rules (" ++ f ls ++ " rebuilt in the last run)."
    ,let f = show . sum . map (length . prfTraces) in "* Building required " ++ f xs ++ " traced commands (" ++ f ls ++ " in the last run)."
    ,"* The total (unparallelised) time is " ++ showDuration (sum $ map prfExecution xs) ++
        " of which " ++ showDuration (sum $ map prfTime $ concatMap prfTraces xs) ++ " is traced commands."
    ,let f xs = if null xs then "0s" else (\(a,b) -> showDuration a ++ " (" ++ b ++ ")") $ maximumBy' (compare `on` fst) xs in
        "* The longest rule takes " ++ f (map (prfExecution &&& prfName) xs) ++
        ", and the longest traced command takes " ++ f (map (prfTime &&& prfCommand) $ concatMap prfTraces xs) ++ "."
    ,let sumLast = sum $ map prfTime $ concatMap prfTraces ls
         maxStop = maximum $ 0 : map prfStop (concatMap prfTraces ls) in
        "* Last run gave an average parallelism of " ++ showDP 2 (if maxStop == 0 then 0 else sumLast / maxStop) ++
        " times over " ++ showDuration maxStop ++ "."
    ]
    where ls = filter ((==) 0 . prfBuilt) xs


generateHTML :: [ProfileEntry] -> IO LBS.ByteString
generateHTML xs = do
    htmlDir <- getDataFileName "html"
    report <- LBS.readFile $ htmlDir </> "profile.html"
    let f name | name == "profile-data.js" = return $ LBS.pack $ "var profile =\n" ++ generateJSON xs
               | name == "version.js" = return $ LBS.pack $ "var version = " ++ show (showVersion version)
               | otherwise = LBS.readFile $ htmlDir </> name
    runTemplate f report


generateTrace :: [ProfileEntry] -> String
generateTrace xs = jsonListLines $
    showEntries 0 [y{prfCommand=prfName x} | x <- xs, y <- prfTraces x] ++
    showEntries 1 (concatMap prfTraces xs)
    where
        showEntries pid xs = map (showEntry pid) $ snd $ mapAccumL alloc [] $ sortBy (compare `on` prfStart) xs
        alloc as r | (a1,an:a2) <- break (\a -> prfStop a <= prfStart r) as = (a1++r:a2, (length a1,r))
                   | otherwise = (as++[r], (length as,r))
        showEntry pid (tid, ProfileTrace{..}) = jsonObject
            [("args","{}"), ("ph",show "X"), ("cat",show "target")
            ,("name",show prfCommand), ("tid",show tid), ("pid",show pid)
            ,("ts",show $ 1000000*prfStart), ("dur",show $ 1000000*(prfStop-prfStart))]


generateJSON :: [ProfileEntry] -> String
generateJSON = jsonListLines . map showEntry
    where
        showEntry ProfileEntry{..} = jsonObject $
            [("name", show prfName)
            ,("built", show prfBuilt)
            ,("changed", show prfChanged)
            ,("depends", show prfDepends)
            ,("execution", showDP 4 prfExecution)] ++
            [("traces", jsonList $ map showTrace prfTraces) | not $ null prfTraces]
        showTrace ProfileTrace{..} = jsonObject
            [("command",show prfCommand), ("start",show prfStart), ("stop",show prfStop)]

jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
jsonObject xs = "{" ++ intercalate "," [show a ++ ":" ++ b | (a,b) <- xs] ++ "}"
