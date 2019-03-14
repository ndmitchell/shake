{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

import Data.Char
import Data.Function
import System.Directory.Extra
import System.Environment
import System.Time.Extra
import System.Info.Extra
import Data.Functor
import Data.List
import Data.Maybe
import Control.Exception.Extra
import Control.Monad
import System.Process.Extra


requiresShake = words "ghc-make shake-language-c"

ms x = show $ ceiling $ x * 1000

cmd :: String -> IO Seconds
cmd x = do
    putStrLn $ "$ " ++ x
    (t, _) <- duration $ system_ x
    putStrLn $ "Took " ++ showDuration t
    return t

main = do
    args <- getArgs
    unless (null args) $ error "Terminating early"

    -- check the TypeScript pieces
    unless isMac $
        void $ cmd "sudo apt-get --allow-unauthenticated install nodejs"
    cmd "npm install -g typescript"
    cmd "tsc --project html/ts"
    cmd "npm install -g tslint"
    cmd "tslint --project html/ts"

    -- grab ninja
    cmd "git clone https://github.com/martine/ninja"
    cmd "cd ninja && ./configure.py --bootstrap"
    copyFile "ninja/ninja" "nin"

    withCurrentDirectory "ninja" $ do

        replicateM_ 3 $ do
            ninjaVer <- cmd "../nin --version"
            shakeVer <- cmd "shake --version --verbose"
            shakeVer <- cmd "shake --version --verbose"
            shakeVer <- cmd "shake --version --quiet"
            shakeVer <- cmd "shake --version"
            shakeVer <- cmd "shake --version"
            putStrLn $ "--version for Ninja is " ++ ms ninjaVer ++ ", for Shake is " ++ ms shakeVer

        cmd "shake --version; shake -C does_not_exist; echo end" -- check for #22

        retry 3 $ do

            -- time Ninja
            cmd "../nin -t clean"
            ninjaFull <- cmd "../nin -j3 -d stats"
            ninjaProfile "build/.ninja_log"
            putStrLn =<< readFile "build/.ninja_log"
            ninjaZero <- cmd "../nin -j3 -d stats"

            cmd "../nin -t clean"
            shakeFull <- cmd "shake -j3 --verbose"

            -- time Shake
            cmd "../nin -t clean"
            shakeFull <- cmd "shake -j3 --quiet --timings"
            cmd "shake --no-build --report=-"
            shakeZero <- cmd "shake -j3 --quiet --timings"

            -- Diagnostics
            cmd "ls -l .shake* build/.ninja*"
            cmd "shake -VV"
            shakeNone <- cmd "shake --rebuild --skip-commands --timings"
            putStrLn $ "--rebuild --skip-commands took " ++ ms shakeNone

            putStrLn $ "Ninja was " ++ ms ninjaFull ++ " then " ++ ms ninjaZero
            putStrLn $ "Shake was " ++ ms shakeFull ++ " then " ++ ms shakeZero

            when (ninjaFull < shakeFull) $
                error "ERROR: Ninja build was faster than Shake"

            when (ninjaZero + 0.1 < shakeZero) $
                error "ERROR: Ninja zero build was more than 0.1s faster than Shake"

    createDirectoryIfMissing True "temp"
    withCurrentDirectory "temp" $
        cmd "shake --demo --keep-going"

    isHead <- (== Just "1") <$> lookupEnv "GHC_HEAD"
    ghcver <- fromMaybe "" <$> lookupEnv "GHCVER"
    unless isHead $ do
        ver <- do
            src <- readFile "shake.cabal"
            return $ head [dropWhile isSpace x | x <- lines src, Just x <- [stripPrefix "version:" x]]
        forM_ requiresShake $ \x ->
            retry 3 $ cmd $ "cabal install " ++ x ++ " --constraint=shake==" ++ ver

ninjaProfile :: FilePath -> IO ()
ninjaProfile src = do
    src <- readFile src
    let times = [(read start, read stop)
                | start:stop:_ <- nubBy ((==) `on` (!! 3)) $
                        reverse $ map words $ filter (not . isPrefixOf "#") $ lines src]
    let work = sum $ map (uncurry subtract) times
    let last = maximum $ map snd times
    putStrLn $ "Ninja profile report: in " ++ show last ++ " ms did " ++ show work ++ " ms work, ratio of " ++ show (work / last)
