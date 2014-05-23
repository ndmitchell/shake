
import Neil
import System.Directory

main = do
    -- grab ninja
    cmd "git clone https://github.com/martine/ninja"
    cmd "cd ninja && ./bootstrap.py"
    cmd "mkdir bin"
    cmd "cp ninja/ninja nin"
    setCurrentDirectory "ninja"

    let ms x = show $ ceiling $ x * 1000
    replicateM_ 3 $ do
        (ninjaVer, _) <- duration $ cmd "../nin --version"
        (shakeVer, _) <- duration $ cmd "shake --version"
        putStrLn $ "--version for Ninja is " ++ ms ninjaVer ++ ", for Shake is " ++ ms shakeVer

    cmd "shake --version; shake -C does_not_exist; echo end" -- check for #22

    retry 3 $ do

        -- time Ninja
        cmd "../nin -t clean"
        (ninjaFull, _) <- duration $ cmd "../nin -j3 -d stats"
        (ninjaZero, _) <- duration $ cmd "../nin -j3 -d stats"

        -- time Shake
        cmd "../nin -t clean"
        (shakeFull, _) <- duration $ cmd "shake -j3 --quiet --timings"
        cmd "shake --no-build --report=-"
        (shakeZero, _) <- duration $ cmd "shake -j3 --quiet --timings"

        -- Diagnostics
        cmd "ls -l .shake* build/.ninja*"
        cmd "shake -VVVV"
        (shakeNone, _) <- duration $ cmd "shake --always-make --skip-commands --timings"
        putStrLn $ "--always-make --skip-commands took " ++ ms shakeNone

        putStrLn $ "Ninja was " ++ ms ninjaFull ++ " then " ++ ms ninjaZero
        putStrLn $ "Shake was " ++ ms shakeFull ++ " then " ++ ms shakeZero

        when (ninjaFull < shakeFull) $
            error "ERROR: Ninja build was faster than Shake"

        when (ninjaZero + 0.1 < shakeZero) $
            error "ERROR: Ninja zero build was more than 0.1s faster than Shake"
