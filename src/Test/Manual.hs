
module Test.Manual(main) where

import Development.Shake hiding (copyFileChanged)
import Development.Shake.FilePath
import Test.Type
import System.Info.Extra


main = shaken test $ \args obj ->
    action $ liftIO $ error "The 'manual' example should only be used in test mode"

test build obj = do
    -- we use .git as our destination, despite not being a real git repo
    -- so that search tools ignore it, and I don't get dupes for every source file
    let dest = obj ".git"
    copyDirectoryChanged "docs/manual" dest
    copyDirectoryChanged "src/Development" $ dest </> "Development"
    copyDirectoryChanged "src/General" $ dest </> "General"
    copyFileChanged "src/Paths.hs" $ dest </> "Paths_shake.hs"
    let cmdline = if isWindows then "build.bat" else "/bin/sh build.sh"
    () <- cmd [Cwd dest, Shell] cmdline "-j2"
    assertExists $ dest </> "_build/run" <.> exe
    () <- cmd [Cwd dest, Shell] cmdline
    () <- cmd [Cwd dest, Shell] [cmdline,"clean"]
    assertMissing $ dest </> "_build/run" <.> exe
