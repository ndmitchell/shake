
module Examples.Self.Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util

import Control.Monad
import Data.Char
import Data.List


main :: IO ()
main = shaken noTest $ \args obj -> do
    let moduleToFile ext xs = map (\x -> if x == '.' then '/' else x) xs <.> ext
    want $ if null args then [obj "Main.exe"] else args

    let ghc args = do
            askOracle ["ghc-version"]
            askOracle ["ghc-pkg"]
            flags <- askOracle ["ghc-flags"]
            system' "ghc" $ args ++ flags

    obj "/*.exe" *> \out -> do
        src <- readFileLines $ replaceExtension out "deps"
        let os = map (obj . moduleToFile "o") $ "Main":src
        need os
        ghc $ ["-o",out] ++ os

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
        ghc ["-c",hs,"-hide-all-packages","-odir=output/self","-hidir=output/self","-i=output/self"]

    obj ".pkgs" *> \out -> do
        src <- readFile' "shake.cabal"
        writeFileLines out $ sort $ cabalBuildDepends src

    addOracle ["ghc-pkg"] $ do
        (out,_) <- systemOutput "ghc-pkg" ["list","--simple-output"]
        return $ words out

    addOracle ["ghc-version"] $ do
        (out,_) <- systemOutput "ghc" ["--version"]
        return [out]

    addOracle ["ghc-flags"] $ do
        pkgs <- readFileLines $ obj ".pkgs"
        return $ map ("-package=" ++) pkgs


---------------------------------------------------------------------
-- GRAB INFORMATION FROM FILES

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]


-- FIXME: Should actually parse the list from the contents of the .cabal file
cabalBuildDepends :: String -> [String]
cabalBuildDepends _ = words $
    "base transformers binary unordered-containers hashable time old-time bytestring " ++
    "filepath directory process deepseq"
