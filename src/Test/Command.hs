{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Test.Command(main) where

import Control.Applicative
import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Time.Extra
import Control.Monad.Extra
import System.Directory
import Test.Type
import System.Exit
import System.Process
import Data.Tuple.Extra
import Data.List.Extra
import Control.Monad.IO.Class
import System.Info.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Prelude


main = shaken test $ \args obj -> do
    let helper = [toNative $ obj "shake_helper" <.> exe]
    let name !> test = do want [name | null args || name `elem` args]
                          name ~> do need [obj "shake_helper" <.> exe]; test

    let helper_source = unlines
            ["import Control.Concurrent"
            ,"import Control.Monad"
            ,"import System.Directory"
            ,"import System.Environment"
            ,"import System.Exit"
            ,"import System.IO"
            ,"import qualified Data.ByteString.Lazy.Char8 as LBS"
            ,"main = do"
            ,"    args <- getArgs"
            ,"    forM_ args $ \\(a:rg) -> do"
            ,"        case a of"
            ,"            'o' -> putStrLn rg"
            ,"            'e' -> hPutStrLn stderr rg"
            ,"            'x' -> exitFailure"
            ,"            'c' -> putStrLn =<< getCurrentDirectory"
            ,"            'v' -> putStrLn =<< getEnv rg"
            ,"            'w' -> threadDelay $ floor $ 1000000 * (read rg :: Double)"
            ,"            'r' -> LBS.putStr $ LBS.replicate (read rg) 'x'"
            ,"            'i' -> putStr =<< getContents"
            ,"        hFlush stdout"
            ,"        hFlush stderr"
            ]

    obj "shake_helper.hs" %> \out -> do need ["src/Test/Command.hs"]; writeFileChanged out helper_source
    [obj "shake_helper" <.> exe, obj "shake_helper.o", obj "shake_helper.hi"] &%> \_ -> do
      need [obj "shake_helper.hs"]; cmd (Cwd $ obj "") "ghc --make" "shake_helper.hs -o shake_helper"

    "capture" !> do
        (Stderr err, Stdout out) <- cmd helper ["ostuff goes here","eother stuff here"]
        liftIO $ out === "stuff goes here\n"
        liftIO $ err === "other stuff here\n"
        liftIO $ waits $ \w -> do
            Stdouterr out <- cmd helper Shell ["o1",w,"e2",w,"o3"]
            out === "1\n2\n3\n"

    "failure" !> do
        (Exit e, Stdout (), Stderr ()) <- cmd helper "oo ee x"
        when (e == ExitSuccess) $ error "/= ExitSuccess"
        liftIO $ assertException ["BAD"] $ cmd helper "oo eBAD x" (EchoStdout False) (EchoStderr False)
        liftIO $ assertException ["MORE"] $ cmd helper "oMORE eBAD x" (WithStdout True) (WithStderr False) (EchoStdout False) (EchoStderr False)

    "cwd" !> do
        -- FIXME: Linux searches the Cwd argument for the file, Windows searches getCurrentDirectory
        let c = liftIO . canonicalizePath
        helper <- c $ obj "shake_helper" <.> exe
        Stdout out <- cmd (Cwd $ obj "") helper "c"
        p0 <- c $ trim out
        p1 <- c $ dropTrailingPathSeparator $ obj ""
        liftIO $ p0 === p1

    "timeout" !> do
        opts <- getShakeOptions
        when (shakeLint opts /= Just LintTracker) $ do
            offset <- liftIO offsetTime
            Exit exit <- cmd (Timeout 2) helper "w20"
            t <- liftIO offset
            putNormal $ "Timed out in " ++ showDuration t
            when (exit == ExitSuccess) $ error "== ExitSuccess"
            when (t < 2 || t > 8) $ error $ "failed to timeout, took " ++ show t

    "env" !> do
        -- use liftIO since it blows away PATH which makes lint-tracker stop working
        Stdout out <- liftIO $ cmd (Env [("FOO","HELLO SHAKE")]) Shell helper "vFOO"
        liftIO $ out === "HELLO SHAKE\n"
        Stdout out <- cmd (AddEnv "FOO" "GOODBYE SHAKE") Shell helper "vFOO"
        liftIO $ out === "GOODBYE SHAKE\n"

    "path" !> do
        let path = AddPath [dropTrailingPathSeparator $ obj ""] []
        unit $ cmd $ obj "shake_helper"
        unit $ cmd $ obj "shake_helper" <.> exe
        unit $ cmd path Shell "shake_helper"
        unit $ cmd path "shake_helper"

    "file" !> do
        let file = obj "file.txt"
        unit $ cmd helper (FileStdout file) (FileStderr file) (EchoStdout False) (EchoStderr False) (WithStderr False) "ofoo ebar obaz"
        liftIO $ assertContents file "foo\nbar\nbaz\n"
        liftIO $ waits $ \w -> do
            Stderr err <- cmd helper (FileStdout file) (FileStderr file) ["ofoo",w,"ebar",w,"obaz"]
            err === "bar\n"
            assertContents file "foo\nbar\nbaz\n"

    "timer" !> do
        timer $ cmd helper

    "binary" !> do
        (Stdout str, Stdout bs) <- cmd BinaryPipes helper "ofoo"
        liftIO $ (===) (str, bs) $ second BS.pack $ dupe $ if isWindows then "foo\r\n" else "foo\n"
        (Stdout str, Stdout bs) <- cmd helper "ofoo"
        liftIO $ (str, bs) === ("foo\n", BS.pack $ if isWindows then "foo\r\n" else "foo\n")
        return ()

    "large" !> do
        (Stdout (_ :: String), CmdTime t1) <- cmd helper "r10000000"
        (Stdout (_ :: LBS.ByteString), CmdTime t2) <- cmd helper "r10000000"
        t3 <- withTempFile $ \file -> fromCmdTime <$> cmd helper "r10000000" (FileStdout file)
        liftIO $ putStrLn $ "Capturing 10Mb takes: " ++ intercalate ","
            [s ++ " = " ++ showDuration d | (s,d) <- [("String",t1),("ByteString",t2),("File",t3)]]

    "stdin" !> do
        Stdout (x :: String) <- cmd helper "i" (Stdin "hello ") (StdinBS $ LBS.pack "world")
        liftIO $ x === "hello world"

    "async" !> do
        let file = obj "async.txt"
        pid <- cmd helper (FileStdout file) "w2" "ohello"
        Nothing <- liftIO $ getProcessExitCode pid
        ExitSuccess <- liftIO $ waitForProcess pid
        liftIO $ assertContents file "hello\n"


test build obj = do
    -- reduce the overhead by running all the tests in parallel
    build ["-j4"]


timer :: (CmdResult r, MonadIO m) => (forall r . CmdResult r => m r) -> m r
timer act = do
    (CmdTime t, CmdLine x, r) <- act
    liftIO $ putStrLn $ "Command " ++ x ++ " took " ++ show t ++ " seconds"
    return r

waits :: (String -> IO ()) -> IO ()
waits op = f 0
    where f w | w > 1 = op "w10"
              | otherwise = catch_ (op $ "w" ++ show w) $ const $ f $ w + 0.1
