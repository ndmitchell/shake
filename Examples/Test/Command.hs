{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Test.Command(main) where

import Development.Shake
import Examples.Util


main = shaken test $ \args obj -> do
    want $ map obj args

    let a !> b = obj a *> \out -> do alwaysRerun; res <- b; writeFile' out res

    "ghc-version" !> do
        Stdout stdout <- cmd "ghc --version"
        return stdout

    "ghc-random" !> do
        (Stderr stderr, Exit _) <- cmd "ghc --random"
        return stderr

    "ghc-random2" !> do
        () <- cmd (EchoStderr False) "ghc --random"
        return ""

    "triple" !> do
        (Exit exit, Stdout stdout, Stderr stderr) <- cmd "ghc --random"
        return $ show (exit, stdout, stderr) -- must force all three parts

    "pwd" !> do
        writeFileLines (obj "pwd space.hs") ["import System.Directory","main = putStrLn =<< getCurrentDirectory"]
        Stdout out <- cmd (Cwd $ obj "") "runhaskell" ["pwd space.hs"]
        return out

    "env" !> do
        (Exit _, Stdout out1) <- cmd (Env [("FOO","HELLO SHAKE")]) Shell "echo %FOO%"
        (Exit _, Stdout out2) <- liftIO $ cmd (Env [("FOO","HELLO SHAKE")]) Shell "echo $FOO"
        return $ unlines [out1, out2]


test build obj = do
    let crash args parts = assertException parts (build $ "--quiet" : args)

    build ["ghc-version"]
    assertContentsInfix (obj "ghc-version") "The Glorious Glasgow Haskell Compilation System"

    build ["ghc-random"]
    assertContentsInfix (obj "ghc-random") "unrecognised flags: --random"

    crash ["ghc-random2"] ["unrecognised flags: --random"]

    build ["pwd"]
    assertContentsInfix (obj "pwd") "command"

    build ["env"]
    assertContentsInfix (obj "env") "HELLO SHAKE"

    build ["triple"]
