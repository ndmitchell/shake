
module Development.Shake.Util(
    parseMakefile, needMakefileDependencies
    ) where

import Development.Shake
import Data.List


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = concatMap f . join . lines
    where
        join (x1:x2:xs) | "\\" `isSuffixOf` x1 = join $ (init x1 ++ x2) : xs
        join (x:xs) = x : join xs
        join [] = []

        f x = [(a, words $ drop 1 b) | a <- words a]
            where (a,b) = break (== ':') $ takeWhile (/= '#') x


needMakefileDependencies :: FilePath -> Action ()
needMakefileDependencies file = need . concatMap snd . parseMakefile =<< liftIO (readFile file)

