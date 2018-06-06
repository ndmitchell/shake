{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}

module Test.SelfMake(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util
import Test.Type

import Control.Applicative
import Control.Monad.Extra
import Data.List.Extra
import System.Info
import Data.Version.Extra
import Prelude


newtype GhcPkg = GhcPkg () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GhcFlags = GhcFlags () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult GhcPkg = [String]
type instance RuleResult GhcFlags = [String]

main = shakeTest_ noTest $ do
    want ["Main" <.> exe]

    ghcPkg <- addOracleHash $ \GhcPkg{} -> do
        Stdout out <- quietly $ cmd "ghc-pkg list --simple-output"
        return $ words out

    ghcFlags <- addOracleHash $ \GhcFlags{} ->
        map ("-package=" ++) <$> readFileLines ".pkgs"

    let ghc args = do
            -- since ghc-pkg includes the ghc package, it changes if the version does
            ghcPkg $ GhcPkg ()
            flags <- ghcFlags $ GhcFlags ()
            cmd "ghc" flags args

    "Main" <.> exe %> \out -> do
        let run = root </> "src/Run.hs"
        copyFileChanged (root </> "src" </> "Paths.hs") "Paths_shake.hs"
        let flags =
                ["-i" ++ root </> "src","-dep-suffix=.","-main-is","Run.main"
                ,"-hide-all-packages","-outputdir=."
                ,"-DPORTABLE","-fwarn-unused-imports","-Werror"] -- to test one CPP branch

        trackAllow ["**/*.o"]
        ghc $ ["-M",run] ++ flags
        need . filter (\x -> takeExtension x == ".hs") . concatMap snd . parseMakefile =<< liftIO (readFile "Makefile")
        ghc $ ["-o",out,run] ++ ["-j4" | compilerVersion >= makeVersion [7,8]] ++ flags

    ".pkgs" %> \out -> do
        src <- readFile' $ root </> "shake.cabal"
        writeFileLines out $ sort $ cabalBuildDepends src


---------------------------------------------------------------------
-- GRAB INFORMATION FROM FILES

-- FIXME: Should actually parse the list from the contents of the .cabal file
cabalBuildDepends :: String -> [String]
cabalBuildDepends _ = packages ++ ["unix" | os /= "mingw32"]

packages = words
    ("base transformers binary unordered-containers hashable time bytestring primitive " ++
     "filepath directory process deepseq random utf8-string extra js-jquery js-flot") ++
    ["old-time" | compilerVersion < makeVersion [7,6]] ++
    ["semigroups" | compilerVersion < makeVersion [8,0]]
