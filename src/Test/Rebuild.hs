{-# LANGUAGE ViewPatterns #-}

module Test.Rebuild(main) where

import Development.Shake
import Test.Type
import General.GetOpt


opts = [Option "" ["timestamp"] (ReqArg Right "VALUE") "Value used to detect what has rebuilt when"]

main = shakeTest test opts $ \(concat -> x) -> do
    want ["a.txt"]
    "a.txt" %> \out -> do
        src <- readFile' "b.txt"
        writeFile' out $ src ++ x

    "b.txt" %> \out -> do
        src <- readFile' "c.txt"
        writeFile' out $ src ++ x

test build = do
    build ["clean"]
    let go arg c b a flags = do
            writeFileChanged "c.txt" c
            build $ ["--timestamp=" ++ arg, "--sleep","--no-reports"] ++ flags
            assertContents "b.txt" b
            assertContents "a.txt" a

    -- check rebuild works
    go "1" "x" "x1" "x11" []
    go "2" "x" "x1" "x11" []
    go "3" "x" "x1" "x13" ["--rebuild=a.*"]
    go "4" "x" "x1" "x13" []
    go "5" "x" "x5" "x55" ["--rebuild=b.*"]
    go "6" "x" "x6" "x66" ["--rebuild"]
    go "7" "x" "x6" "x66" []
    go "8" "y" "y8" "y88" []

    -- check skip works
    go "1" "x" "x1" "x11" []
    go "2" "y" "y2" "x11" ["--skip=a.*"]
    go "3" "y" "y2" "y23" []
    go "4" "z" "y2" "y23" ["--skip=b.*"]
    go "5" "z" "y2" "y23" ["--skip=b.*"]
    go "6" "z" "z6" "z66" []
    go "7" "a" "z6" "z66" ["--skip=c.*"]
    go "8" "a" "z6" "z66" ["--skip=b.*"]
    go "9" "a" "a9" "z66" ["--skip=a.*"]
    go "0" "a" "a9" "a90" []

{-
    -- check skip-forever works
    -- currently it does not work properly
    go "1" "x" "x1" "x11" []
    go "2" "y" "y2" "x11" ["--skip-forever=a.*"]
    go "3" "y" "y2" "x11" []
    go "4" "z" "z4" "z44" []
-}
