{-# LANGUAGE PatternGuards #-}

module Development.Shake.Report(buildReport) where

import Control.Monad
import System.FilePath
import Paths_shake


-- | Generates an HTML report given some build system
--   profiling data in JSON format.
buildReport :: String -> FilePath -> IO ()
buildReport json out = do
    htmlDir <- getDataFileName "html"
    report <- readFile $ htmlDir </> "report.html"
    let f name | name == "data" = return json
               | otherwise = readFile $ htmlDir </> name
    writeFile out =<< runTemplate f report


-- | Template Engine
runTemplate :: Monad m => (String -> m String) -> String -> m String
runTemplate ask ('$':'$':xs) = liftM ('$':) $ runTemplate ask xs
runTemplate ask ('$':xs) | (name,_:rest) <- break (== '$') xs = liftM2 (++) (ask name) (runTemplate ask rest)
runTemplate ask (x:xs) = liftM (x:) $ runTemplate ask xs
runTemplate ask [] = return []
