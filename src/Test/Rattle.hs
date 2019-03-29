
module Test.Rattle(main) where

import Development.Rattle
import System.FilePattern.Directory
import Development.Shake.FilePath
import Control.Monad
import Test.Type

main = testSimple $ rattle $ do
    cs <- liftIO $ getDirectoryFiles "." [shakeRoot </> "src/Test/C/*.c"]
    let toO x = takeBaseName x <.> "o"
    forM_ cs $ \c -> cmd ["gcc","-o",toO c,"-c",c]
    cmd $ ["gcc","-o","Main.exe"] ++ map toO cs
    cmd ["Main.exe"]
