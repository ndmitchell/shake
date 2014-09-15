
import Neil
import System.Directory
import Data.Char
import Data.Function

requiresShake = words "ghc-make"

ms x = show $ ceiling $ x * 1000

main = do
    -- grab ninja
    cmd "git clone https://github.com/martine/ninja"
    cmd "cd ninja && ./bootstrap.py"
    cmd "mkdir bin"
    cmd "cp ninja/ninja nin"
    setCurrentDirectory "ninja"

    replicateM_ 3 $ do
        (ninjaVer, _) <- duration $ cmd "../nin --version"
        (shakeVer, _) <- duration $ cmd "shake --version"
        putStrLn $ "--version for Ninja is " ++ ms ninjaVer ++ ", for Shake is " ++ ms shakeVer

    cmd "shake --version; shake -C does_not_exist; echo end" -- check for #22

    retry 3 $ do

        -- time Ninja
        cmd "../nin -t clean"
        (ninjaFull, _) <- duration $ cmd "../nin -j3 -d stats"
        ninjaProfile "build/.ninja_log"
        putStrLn =<< readFile "build/.ninja_log"
        (ninjaZero, _) <- duration $ cmd "../nin -j3 -d stats"

        -- time Shake
        cmd "../nin -t clean"
        (shakeFull, _) <- duration $ cmd "shake -j3 --quiet --timings"
        cmd "shake --no-build --report=-"
        (shakeZero, _) <- duration $ cmd "shake -j3 --quiet --timings"

        -- Diagnostics
        cmd "ls -l .shake* build/.ninja*"
        cmd "shake -VV"
        (shakeNone, _) <- duration $ cmd "shake --always-make --skip-commands --timings"
        putStrLn $ "--always-make --skip-commands took " ++ ms shakeNone

        putStrLn $ "Ninja was " ++ ms ninjaFull ++ " then " ++ ms ninjaZero
        putStrLn $ "Shake was " ++ ms shakeFull ++ " then " ++ ms shakeZero

        when (ninjaFull < shakeFull) $
            error "ERROR: Ninja build was faster than Shake"

        when (ninjaZero + 0.1 < shakeZero) $
            error "ERROR: Ninja zero build was more than 0.1s faster than Shake"

    setCurrentDirectory ".."
    cmd "ghc -threaded -rtsopts -isrc -i. Paths.hs Main.hs --make -O -prof -auto-all -caf-all"
    setCurrentDirectory "ninja"
    putStrLn "== PROFILE BUILDING FROM SCRATCH =="
    cmd "rm .shake*"
    cmd "../Main --skip-commands +RTS -p -V0.001"
    cmd "head -n32 Main.prof"

    putStrLn "== PROFILE BUILDING NOTHING =="
    cmd "../Main +RTS -p -V0.001"
    cmd "head -n32 Main.prof"
    setCurrentDirectory ".."

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
