
module Examples.Self.Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util

import Control.Monad
import Data.Char
import Data.List


main :: IO ()
main = shaken "self" $ \obj -> do
    let pkgs = "transformers binary unordered-containers parallel-io filepath directory process"
        flags = map ("-package=" ++) $ words pkgs

    let moduleToFile ext xs = map (\x -> if x == '.' then '/' else x) xs <.> ext
    want [obj "Main.exe"]

    obj "/*.exe" *> \out -> do
        src <- readFileLines $ replaceExtension out "deps"
        let os = map (obj . moduleToFile "o") $ "Main":src
        need os
        system' "ghc" $ ["-o",out] ++ os ++ flags

    obj "/*.deps" *> \out -> do
        dep <- readFileLines $ replaceExtension out "dep"
        let xs = map (obj . moduleToFile "deps") dep
        need xs
        ds <- fmap (nub . sort . (++) dep . concat) $ mapM readFileLines xs
        writeFileLines out ds

    obj "/*.dep" *> \out -> do
        src <- readFile' $ unobj $ replaceExtension out "hs"
        let xs = hsImports src
        xs <- filterM (doesFileExist . moduleToFile "hs") xs
        writeFileLines out xs

    obj "/*.hi" *> \out -> do
        need [replaceExtension out "o"]

    obj "/*.o" *> \out -> do
        dep <- readFileLines $ replaceExtension out "dep"
        let hs = unobj $ replaceExtension out "hs"
        need $ hs : map (obj . moduleToFile "hi") dep
        system' "ghc" $ ["-c",hs,"-odir=output/self","-hidir=output/self","-i=output/self"] ++ flags


hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]
