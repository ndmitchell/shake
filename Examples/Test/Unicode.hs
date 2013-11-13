{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Test.Unicode(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import Control.Monad
import System.Directory(createDirectoryIfMissing)


-- | Decode a dull ASCII string to certain unicode points, necessary because
--   withArgs (even the UTF8 version) throws an encoding error on the > 256 code points
decode :: String -> String
decode ('e':'^':xs) = '\xEA' : decode xs -- Latin Small Letter E With Circumflex
decode (':':')':xs) = '\x263A' : decode xs -- White Smiling Face
decode (x:xs) = x : decode xs
decode [] = []


main = shaken test $ \xs obj -> do
    let pre:args = map decode xs
    want $ map obj args

    obj (pre ++ "dir/*") *> \out -> do
        let src = takeDirectory (takeDirectory out) </> takeFileName out
        copyFile' src out

    obj (pre ++ ".out") *> \out -> do
        a <- readFile' $ obj $ pre ++ "dir" </> pre <.> "source"
        b <- readFile' $ obj pre <.> "multi1"
        writeFile' out $ a ++ b

    map obj ["*.multi1","*.multi2"] *>> \[m1,m2] -> do
        b <- doesFileExist $ m1 -<.> "exist"
        writeFile' m1 $ show b
        writeFile' m2 $ show b


test build obj = do
    build ["clean"]
    -- Useful, if the error message starts crashing...
    -- IO.hSetEncoding IO.stdout IO.char8
    -- IO.hSetEncoding IO.stderr IO.char8
    forM_ ["normal","e^",":)","e^-:)"] $ \pre -> do
        createDirectoryIfMissing True $ obj ""
        let ext x = obj $ decode pre <.> x
        writeFile (ext "source") "x"
        build [pre,pre <.> "out","--sleep"]
        assertContents (ext "out") $ "x" ++ "False"
        writeFile (ext "source") "y"
        build [pre,pre <.> "out","--sleep"]
        assertContents (ext "out") $ "y" ++ "False"
        writeFile (ext "exist") ""
        build [pre,pre <.> "out"]
        assertContents (ext "out") $ "y" ++ "True"
