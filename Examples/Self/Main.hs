
module Examples.Self.Main(main) where

import Development.Shake
import Development.Shake.FilePath

import Control.Monad
import Data.Char
import Data.List


main :: IO ()
main = shake shakeOptions{shakeFiles="output/self", shakeVerbosity=2, shakeParallelism=2} $ do
    let pkgs = "transformers binary unordered-containers parallel-io"
        flags = map ("-package=" ++) $ words pkgs

    let out x = "output/self/" ++ x
        unout x = dropDirectory1 $ dropDirectory1 x
        moduleToFile ext xs = map (\x -> if x == '.' then '/' else x) xs <.> ext
    want [out "Main.exe"]

    out "/*.exe" *> \res -> do
        src <- readFileLines $ replaceExtension res "deps"
        let os = map (out . moduleToFile "o") $ "Main":src
        need os
        system_ $ ["ghc","-o",res] ++ os ++ flags

    out "/*.deps" *> \res -> do
        dep <- readFileLines $ replaceExtension res "dep"
        let xs = map (out . moduleToFile "deps") dep
        need xs
        ds <- fmap (nub . sort . (++) dep . concat) $ mapM readFileLines xs
        writeFileLines res ds

    out "/*.dep" *> \res -> do
        src <- readFile_ $ unout $ replaceExtension res "hs"
        let xs = hsImports src
        xs <- filterM (doesFileExist . moduleToFile "hs") xs
        writeFileLines res xs

    out "/*.o" *> \res -> do
        need [replaceExtension res "hi"]

    out "/*.hi" *> \res -> do
        dep <- readFileLines $ replaceExtension res "dep"
        let hs = unout $ replaceExtension res "hs"
        need $ hs : map (out . moduleToFile "hi") dep
        system_ $ ["ghc","-c",hs,"-odir=output/self","-hidir=output/self","-i=output/self"] ++ flags


hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]
