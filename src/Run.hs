
module Run(main) where

import Development.Make.All
import Development.Ninja.All
import System.Environment
import Development.Shake
import Development.Shake.FilePath
import General.Timing
import Data.Maybe
import qualified System.Directory as IO
import System.Console.GetOpt


main :: IO ()
main = do
    resetTimings
    args <- getArgs
    withArgs ("--no-time":args) $
        shakeArgsWith shakeOptions{shakeCreationCheck=False} flags $ \opts targets -> do
            let tool = listToMaybe [x | Tool x <- opts]
            (mode, makefile) <- case reverse [x | UseMakefile x <- opts] of
                x:_ -> return (modeMakefile x, x)
                _ -> do x <- findMakefile; return (modeMakefile x, x)
            case mode of
                Ninja -> runNinja makefile targets tool
                _ | isJust tool -> error "--tool flag is not supported without a .ninja Makefile"
                _ -> fmap Just $ runMakefile makefile targets


data Flag = UseMakefile FilePath
          | Tool String

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."
        ,Option "t" ["tool"] (ReqArg (Right . Tool) "TOOL") "Ninja-compatible tools."
        ]

data Mode = Make | Ninja

modeMakefile :: FilePath -> Mode
modeMakefile x = if takeExtension x == ".ninja" then Ninja else Make


findMakefile :: IO FilePath
findMakefile = do
    b <- IO.doesFileExist "makefile"
    if b then return "makefile" else do
        b <- IO.doesFileExist "Makefile"
        if b then return "Makefile" else do
            b <- IO.doesFileExist "build.ninja"
            if b then return "build.ninja" else
                error "Could not find `makefile', `Makefile' or `build.ninja'"
