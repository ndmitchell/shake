
module Examples.C.Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util

main = shaken noTest $ \args obj -> do
    let src = "Examples/C"
    want [obj "Main.exe"]

    obj "Main.exe" *> \out -> do
        cs <- getDirectoryFiles src "*.c"
        let os = map (obj . (<.> "o")) cs
        need os
        system' "gcc" $ ["-o",out] ++ os

    obj "*.c.o" *> \out -> do
        let c = src </> takeBaseName out
        need [c]
        headers <- cIncludes c
        need $ map ((</>) src . takeFileName) headers
        system' "gcc" ["-o",out,"-c",c]

cIncludes :: FilePath -> Action [FilePath]
cIncludes x = do
    (stdout,_) <- systemOutput "gcc" ["-MM",x]
    return $ drop 2 $ words stdout
