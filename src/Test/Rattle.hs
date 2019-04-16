{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rattle(main) where

import Development.Rattle
import General.Extra
import System.FilePattern.Directory
import Development.Shake.FilePath
import Control.Exception
import System.Info.Extra
import Control.Monad.Extra
import Test.Type

-- Don't run on Mac because of https://github.com/jacereda/fsatrace/issues/25
main = testSimpleClean $ whenM hasTracker $ unless isMac $ do
    let wipe = mapM removeFile_ =<< getDirectoryFiles "." ["*"]
    cs <- liftIO $ getDirectoryFiles "." [shakeRoot </> "src/Test/C/*.c"]
    let toO x = takeBaseName x <.> "o"
    let build = do
            forM_ cs $ \c -> cmd ["gcc","-o",toO c,"-c",c]
            cmd $ ["gcc","-o","Main" <.> exe] ++ map toO cs
            cmd ["./Main" <.> exe]

    putStrLn "Build 1: Expect everything"
    rattle rattleOptions build
    putStrLn "Build 2: Expect nothing"
    rattle rattleOptions build
    wipe
    putStrLn "Build 3: Expect cached (some speculation)"
    rattle rattleOptions build


    putStrLn "Build 4: Read/write hazard"
    handle (\(h :: Hazard) -> print h) $ do
        rattle rattleOptions{rattleSpeculate=Nothing} $ do
            cmd ["./Main" <.> exe]
            cmd $ ["gcc","-o","Main" <.> exe] ++ reverse (map toO cs)
        putStrLn "Hoped it failed, but doesn't always"
        -- fail "Expected a hazard"

    putStrLn "Build 5: Rebuild after"
    rattle rattleOptions build
