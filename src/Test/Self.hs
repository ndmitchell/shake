{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Test.Self(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Test.Type

import Control.Monad
import Data.Char
import Data.List.Extra
import System.Info


newtype GhcPkg = GhcPkg () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GhcFlags = GhcFlags () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)


main = shaken noTest $ \args obj -> do
    let moduleToFile ext xs = map (\x -> if x == '.' then '/' else x) xs <.> ext
    want $ if null args then [obj "Main" <.> exe] else args

    -- fixup to cope with Cabal's generated files
    let fixPaths x = if x == "Paths_shake.hs" then "Paths.hs" else x

    ghcPkg <- addOracle $ \GhcPkg{} -> do
        Stdout out <- quietly $ cmd "ghc-pkg list --simple-output"
        return $ words out

    ghcFlags <- addOracle $ \GhcFlags{} -> do
        pkgs <- readFileLines $ obj ".pkgs"
        return $ map ("-package=" ++) pkgs

    let ghc args = do
            -- since ghc-pkg includes the ghc package, it changes if the version does
            ghcPkg $ GhcPkg ()
            flags <- ghcFlags $ GhcFlags ()
            cmd "ghc" flags args

    obj "Main" <.> exe %> \out -> do
        src <- readFileLines $ obj "Run.deps"
        let os = map (obj . moduleToFile "o") $ "Run" : src
        need os
        ghc $ ["-o",out] ++ os

    obj "//*.deps" %> \out -> do
        dep <- readFileLines $ out -<.> "dep"
        let xs = map (obj . moduleToFile "deps") dep
        need xs
        ds <- fmap (nub . sort . (++) dep . concat) $ mapM readFileLines xs
        writeFileLines out ds

    obj "//*.dep" %> \out -> do
        src <- readFile' $ "src" </> fixPaths (unobj $ out -<.> "hs")
        let xs = hsImports src
        xs <- filterM (doesFileExist . ("src" </>) . fixPaths . moduleToFile "hs") xs
        writeFileLines out xs

    [obj "//*.o",obj "//*.hi"] &%> \[out,_] -> do
        deps <- readFileLines $ out -<.> "deps"
        let hs = "src" </> fixPaths (unobj $ out -<.> "hs")
        need $ hs : map (obj . moduleToFile "hi") deps
        ghc $ ["-c",hs,"-isrc","-main-is","Run.main"
              ,"-hide-all-packages","-odir=output/self","-hidir=output/self","-i=output/self"] ++
              ["-DPORTABLE","-fwarn-unused-imports","-Werror"] -- to test one CPP branch

    obj ".pkgs" %> \out -> do
        src <- readFile' "shake.cabal"
        writeFileLines out $ sort $ cabalBuildDepends src


---------------------------------------------------------------------
-- GRAB INFORMATION FROM FILES

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- concatMap (wordsBy (== ';')) $ lines xs, "import " `isPrefixOf` trim x]


-- FIXME: Should actually parse the list from the contents of the .cabal file
cabalBuildDepends :: String -> [String]
cabalBuildDepends _ = packages ++ ["unix" | os /= "mingw32"]

packages = words $
    "base transformers binary unordered-containers hashable time old-time bytestring " ++
    "filepath directory process deepseq random utf8-string extra js-jquery js-flot"
