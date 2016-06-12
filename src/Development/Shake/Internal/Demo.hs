
-- | Demo tutorial, accessed with --demo
module Development.Shake.Internal.Demo(demo) where

import Paths_shake
import Development.Shake.Command

import Control.Applicative
import Control.Exception.Extra
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Version(showVersion)
import System.Directory
import System.Exit
import System.FilePath
import Development.Shake.FilePath(exe)
import System.IO
import System.Info.Extra
import Prelude


demo :: Bool -> IO ()
demo auto = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "% Welcome to the Shake v" ++ showVersion version ++ " demo mode!"
    putStr "% Detecting machine configuration... "

    -- CONFIGURE

    manual <- getDataFileName "docs/manual"
    hasManual <- wrap $ doesDirectoryExist manual
    ghc <- findExecutable "ghc"
    gcc <- do
        v <- findExecutable "gcc"
        case v of
            Nothing | isWindows, Just ghc <- ghc -> do
                let dir = takeDirectory (takeDirectory ghc) </> "bin/mingw/gcc.exe"
                b <- wrap $ doesFileExist dir
                return $ if b then Just dir else Nothing
            _ -> return v
    shakeLib <- wrap $ fmap (not . null . words . fromStdout) (cmd "ghc-pkg list --simple-output shake")
    ninja <- findExecutable "ninja"
    putStrLn "done\n"

    let path = if isWindows then "%PATH%" else "$PATH"
    require (isJust ghc) $ "% You don't have 'ghc' on your " ++ path ++ ", which is required to run the demo."
    require (isJust gcc) $ "% You don't have 'gcc' on your " ++ path ++ ", which is required to run the demo."
    require shakeLib "% You don't have the 'shake' library installed with GHC, which is required to run the demo."
    require hasManual "% You don't have the Shake data files installed, which are required to run the demo."

    empty <- (not . any (not . all (== '.'))) <$> getDirectoryContents "."
    dir <- if empty then getCurrentDirectory else do
        home <- getHomeDirectory
        dir <- getDirectoryContents home
        return $ home </> head (map ("shake-demo" ++) ("":map show [2..]) \\ dir)

    putStrLn "% The Shake demo uses an empty directory, OK to use:"
    putStrLn $ "%     " ++ dir
    b <- yesNo auto
    require b "% Please create an empty directory to run the demo from, then run 'shake --demo' again."

    putStr "% Copying files... "
    createDirectoryIfMissing True dir
    forM_ ["Build.hs","main.c","constants.c","constants.h","build" <.> if isWindows then "bat" else "sh"] $ \file ->
        copyFile (manual </> file) (dir </> file)
    unless isWindows $ do
         p <- getPermissions $ dir </> "build.sh"
         setPermissions (dir </> "build.sh") p{executable=True}
    putStrLn "done"

    let pause = do
            putStr "% Press ENTER to continue: "
            if auto then putLine "" else getLine
    let execute x = do
            putStrLn $ "% RUNNING: " ++ x
            cmd (Cwd dir) Shell x :: IO ()
    let build = if isWindows then "build" else "./build.sh"

    putStrLn "\n% [1/5] Building an example project with Shake."
    pause
    putStrLn $ "% RUNNING: cd " ++ dir
    execute build

    putStrLn "\n% [2/5] Running the produced example."
    pause
    execute $ "_build" </> "run" <.> exe

    putStrLn "\n% [3/5] Rebuilding an example project with Shake (nothing should change)."
    pause
    execute build

    putStrLn "\n% [4/5] Cleaning the build."
    pause
    execute $ build ++ " clean"

    putStrLn "\n% [5/5] Rebuilding with 2 threads and profiling."
    pause
    execute $ build ++ " -j2 --report --report=-"
    putStrLn "\n% See the profiling summary above, or look at the HTML profile report in"
    putStrLn $ "%     " ++ dir </> "report.html"

    putStrLn "\n% Demo complete - all the examples can be run from:"
    putStrLn $ "%     " ++ dir
    putStrLn "% For more info see http://shakebuild.com"
    when (isJust ninja) $ do
        putStrLn "\n% PS. Shake can also execute Ninja build files"
        putStrLn "% For more info see http://shakebuild.com/ninja"



-- | Require the user to press @y@ before continuing.
yesNo :: Bool -> IO Bool
yesNo auto = do
    putStr "% [Y/N] (then ENTER): "
    x <- if auto then putLine "y" else fmap (map toLower) getLine
    if "y" `isPrefixOf` x then
        return True
     else if "n" `isPrefixOf` x then
        return False
     else
        yesNo auto

putLine :: String -> IO String
putLine x = putStrLn x >> return x


-- | Replace exceptions with 'False'.
wrap :: IO Bool -> IO Bool
wrap act = act `catch_` const (return False)


-- | Require a condition to be true, or exit with a message.
require :: Bool -> String -> IO ()
require b msg = unless b $ putStrLn msg >> exitFailure
