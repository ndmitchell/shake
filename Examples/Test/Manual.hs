
module Examples.Test.Manual(main) where

import Development.Shake
import Development.Shake.FilePath
import Examples.Util
import General.Base


main = shaken test $ \args obj ->
    action $ liftIO $ error "The 'manual' example should only be used in test mode"

test build obj = do
    copyDirectoryChanged "docs/manual" $ obj "manual"
    copyDirectoryChanged "Development" $ obj "manual/Development"
    copyDirectoryChanged "General" $ obj "manual/General"
    copyFileChanged "Paths.hs" $ obj "manual/Paths_shake.hs"
    let cmdline = if isWindows then "build.bat" else "/bin/sh build.sh"
    () <- cmd [Cwd $ obj "manual", Shell] cmdline "-j2"
    assertExists $ obj "manual/_build/run" <.> exe
    () <- cmd [Cwd $ obj "manual", Shell] cmdline
    () <- cmd [Cwd $ obj "manual", Shell] [cmdline,"clean"]
    assertMissing $ obj "manual/_build/run" <.> exe
    return ()
