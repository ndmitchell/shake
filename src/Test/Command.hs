
module Test.Command(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type


main = shaken test $ \args obj -> do
    want $ map obj args

    let a !> b = obj a %> \out -> do alwaysRerun; res <- b; writeFile' out res

    "ghc-version" !> do
        Stdout stdout <- cmd "ghc --version"
        return stdout

    "ghc-random" !> do
        (Stderr stderr, Exit _) <- cmd "ghc --random"
        return stderr

    "ghc-random2" !> do
        () <- cmd (EchoStderr False) (Cwd $ obj "") "ghc --random"
        return ""

    "triple" !> do
        (Exit exit, Stdout stdout, Stderr stderr) <- cmd "ghc --random"
        return $ show (exit, stdout, stderr) -- must force all three parts

    obj "pwd space.hs" %> \out -> writeFileLines out ["import System.Directory","main = putStrLn =<< getCurrentDirectory"]
    "pwd" !> do
        need [obj "pwd space.hs"]
        Stdout out <- cmd (Cwd $ obj "") "runhaskell" ["pwd space.hs"]
        return out

    "env" !> do
        (Exit _, Stdout out1) <- cmd (Env [("FOO","HELLO SHAKE")]) Shell "echo %FOO%"
        (Exit _, Stdout out2) <- liftIO $ cmd (Env [("FOO","HELLO SHAKE")]) Shell "echo $FOO"
        return $ unlines [out1, out2]

    obj "bin/myexe" <.> exe %> \out -> do
        liftIO $ writeFile (obj "myexe.hs") "main = return () :: IO ()"
        cmd "ghc --make" [obj "myexe.hs"] "-o" [out]

    "path_" !> do
        need [obj "bin/myexe" <.> exe]
        fmap fromStdout $ cmd "myexe"

    "path" !> do
        path <- addPath [obj "bin"] []
        () <- cmd $ obj "bin/myexe"
        () <- cmd $ obj "bin/myexe" <.> exe
        () <- cmd path Shell "myexe"
        () <- cmd path "myexe"
        return ""


test build obj = do
    let crash args parts = assertException parts (build $ "--quiet" : args)

    build ["ghc-version"]
    assertContentsInfix (obj "ghc-version") "The Glorious Glasgow Haskell Compilation System"

    build ["ghc-random"]
    assertContentsInfix (obj "ghc-random") "unrecognised flag"
    assertContentsInfix (obj "ghc-random") "--random"

    crash ["ghc-random2"] [obj "","unrecognised flag","--random"]

    build ["pwd"]
    assertContentsInfix (obj "pwd") "command"

    build ["env","--no-lint"] -- since it blows away the $PATH, which is necessary for lint-tracker
    assertContentsInfix (obj "env") "HELLO SHAKE"

    build ["triple"]

    crash ["path_"] ["myexe"]
    build ["path"]
