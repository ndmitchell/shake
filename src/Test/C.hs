
module Test.C(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type

main = shakeTest_ noTest $ do
    let obj = id
    let src = root </> "src/Test/C"
    want [obj "Main.exe"]

    obj "Main.exe" %> \out -> do
        cs <- getDirectoryFiles src ["*.c"]
        let os = map (obj . (<.> "o")) cs
        need os
        cmd "gcc -o" [out] os

    obj "*.c.o" %> \out -> do
        let c = src </> takeBaseName out
        need [c]
        headers <- cIncludes c
        need $ map ((</>) src . takeFileName) headers
        cmd "gcc -o" [out] "-c" [c]

cIncludes :: FilePath -> Action [FilePath]
cIncludes x = do
    Stdout stdout <- cmd "gcc" ["-MM",x]
    return $ drop 2 $ words stdout
