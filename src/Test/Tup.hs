
module Test.Tup(main) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util
import Test.Type
import Control.Applicative
import Data.Maybe
import Prelude


main = shakeTest_ noTest $ do
    -- Example inspired by http://gittup.org/tup/ex_multiple_directories.html
    usingConfigFile $ root </> "src/Test/Tup/root.cfg"

    action $ do
        keys <- getConfigKeys
        need [x -<.> exe | x <- keys, takeExtension x == ".exe"]

    let objects dir key = do
            let f x | takeExtension x == ".c" = dir </> x -<.> "o"
                    | takeExtension x == ".a" = takeBaseName x </> "lib" ++ x
                    | otherwise = error $ "Unknown extension, " ++ x
            x <- fromMaybe (error $ "Missing config key, " ++ key) <$> getConfig key
            return $ map f $ words x

    (\x -> x -<.> exe == x) ?> \out -> do
        os <- objects "" $ takeBaseName out <.> "exe"
        need os
        cmd "gcc" os "-o" [out]

    "//lib*.a" %> \out -> do
        os <- objects (drop 3 $ takeBaseName out) $ drop 3 $ takeFileName out
        need os
        cmd "ar crs" [out] os

    "//*.o" %> \out -> do
        let src = root </> "src/Test/Tup" </> out -<.> "c"
        need [src]
        cmd_ "gcc -c -MMD -MF" [out -<.> "d"] [src] "-o" [out] "-O2 -Wall" ["-I" ++ root </> "src/Test/Tup/newmath"]
        neededMakefileDependencies $ out -<.> "d"
