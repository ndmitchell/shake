
module Test.Tup(main) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util
import Test.Type
import Data.Maybe


main = shaken noTest $ \args obj -> do
    -- Example inspired by http://gittup.org/tup/ex_multiple_directories.html
    usingConfigFile "src/Test/Tup/root.cfg"

    action $ do
        keys <- getConfigKeys
        need [x -<.> exe | x <- keys, takeExtension x == ".exe"]

    let objects dir key = do
        let f x | takeExtension x == ".c" = obj $ dir </> x -<.> "o"
                | takeExtension x == ".a" = obj $ takeBaseName x </> "lib" ++ x
                | otherwise = error $ "Unknown extension, " ++ x
        x <- fmap (fromMaybe $ error $ "Missing config key, " ++ key) $ getConfig key
        return $ map f $ words x

    (\x -> x -<.> exe == x) ?> \out -> do
        os <- objects "" $ takeBaseName out <.> "exe"
        need os
        cmd "gcc" os "-o" [out]

    obj "//lib*.a" %> \out -> do
        os <- objects (drop 3 $ takeBaseName out) $ drop 3 $ takeFileName out
        need os
        cmd "ar crs" [out] os

    obj "//*.o" %> \out -> do
        let src = "src/Test/Tup" </> unobj out -<.> "c"
        need [src]
        () <- cmd "gcc -c -MMD -MF" [out -<.> "d"] [src] "-o" [out] "-O2 -Wall -Isrc/Test/Tup/newmath"
        neededMakefileDependencies $ out -<.> "d"
