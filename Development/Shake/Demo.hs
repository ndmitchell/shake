{-# LANGUAGE ScopedTypeVariables #-}

-- | Demo tutorial, accessed with --demo
module Development.Shake.Demo(demo) where

import Paths_shake
import Development.Shake.Command
import General.Base

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Version(showVersion)
import System.Directory
import System.Exit
import System.FilePath
import System.IO


demo :: IO ()
demo = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "# Welcome to the Shake v" ++ showVersion version ++ " demo mode!"
    putStr $ "# Detecting machine configuration... "

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
    shake <- findExecutable "shake"
    ninja <- findExecutable "ninja"
    putStrLn "done\n"

    let path = if isWindows then "%PATH%" else "$PATH"
    require (isJust ghc) $ "You don't have 'ghc' on your " ++ path ++ ", which is required to run the demo."
    require (isJust gcc) $ "You don't have 'gcc' on your " ++ path ++ ", which is required to run the demo."
    require shakeLib "You don't have the 'shake' library installed with GHC, which is required to run the demo."
    require hasManual "You don't have the Shake data files installed, which are required to run the demo."

    empty <- fmap (null . filter (not . all (== '.'))) $ getDirectoryContents "."
    dir <- if empty then getCurrentDirectory else do
        home <- getHomeDirectory
        dir <- getDirectoryContents home
        return $ home </> head (map ("shake-demo" ++) ("":map show [2..]) \\ dir)

    putStrLn $ "# Shake requires an empty directory for the demo, is it OK to use:"
    putStrLn $ "    " ++ dir ++ "?"
    b <- yesNo
    require b "Please create an empty directory to run the demo from, then run 'shake --demo' again."

    putStr "# Copying files... "
    createDirectoryIfMissing True dir
    forM_ ["Build.hs","main.c","constants.c","constants.h","build" <.> if isWindows then "bat" else "sh"] $ \file ->
        copyFile (manual </> file) (dir </> file)
    putStrLn "done"

    putStr "\n# Shake is about to build an example project, press <ENTER> to continue:"
    getLine
    putStrLn $ "# RUNNING: cd " ++ dir
    putStrLn $ "# RUNNING: " ++ (if isWindows then "build" else "./build.sh")
    () <- cmd (Cwd dir) Shell $ if isWindows then "build.bat" else "./build.sh"

    putStr "\n# Shake is about to rebuild the project (nothing should change), press <ENTER> to continue:"
    getLine
    putStrLn $ "# RUNNING: " ++ (if isWindows then "build" else "./build.sh")
    () <- cmd (Cwd dir) Shell $ if isWindows then "build.bat" else "./build.sh"

    putStr "\n# Shake is going to tell you some profiling statistics, press <ENTER> to continue:"
    getLine
    putStrLn $ "# RUNNING: " ++ (if isWindows then "build" else "./build.sh") ++ " --profile --profile=-"
    () <- cmd (Cwd dir) Shell (if isWindows then "build.bat" else "./build.sh") "--profile --profile=-"

    putStrLn "# Demo complete"


-- | Require the user to press @y@ before continuing.
yesNo :: IO Bool
yesNo = do
    putStr $ "Press y/n followed by <ENTER>. "
    x <- fmap (map toLower) getLine
    if "y" `isPrefixOf` x then
        return True
     else if "n" `isPrefixOf` x then
        return False
     else
        yesNo


-- | Replace exceptions with 'False'.
wrap :: IO Bool -> IO Bool
wrap act = act `catch` \(_ :: SomeException) -> return False


-- | Require a condition to be true, or exit with a message.
require :: Bool -> String -> IO ()
require b msg = unless b $ putStrLn msg >> exitFailure
