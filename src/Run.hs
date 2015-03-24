
module Run(main) where

import Development.Make.All
import Development.Ninja.All
import System.Environment
import Development.Shake
import Development.Shake.FilePath
import General.Timing
import Data.List.Extra
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Maybe
import qualified System.Directory as IO
import System.Console.GetOpt
import System.Process
import System.Exit
import General.Extra


main :: IO ()
main = do
    resetTimings
    args <- getArgs
    withArgs ("--no-time":args) $
        shakeArgsWith shakeOptions{shakeCreationCheck=False} flags $ \opts targets -> do
            let tool = listToMaybe [x | Tool x <- opts]
            (mode, makefile) <- case reverse [x | UseMakefile x <- opts] of
                x:_ -> return (modeMakefile x, x)
                _ -> findMakefile
            case mode of
                Ninja -> runNinja makefile targets tool
                _ | isJust tool -> error "--tool flag is not supported without a .ninja Makefile"
                Exe -> exitWith =<< rawSystem (toNative makefile) args
                Haskell -> exitWith =<< rawSystem "runhaskell" (makefile:args)
                Make -> fmap Just $ runMakefile makefile targets


data Flag = UseMakefile FilePath
          | Tool String

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."
        ,Option "t" ["tool"] (ReqArg (Right . Tool) "TOOL") "Ninja-compatible tools."
        ]

data Mode = Make | Ninja | Haskell | Exe

modeMakefile :: FilePath -> Mode
modeMakefile x | takeExtension x == ".ninja" = Ninja
               | takeExtension x `elem` [".hs",".lhs"] = Haskell
               | otherwise = Make


findMakefile :: IO (Mode, FilePath)
findMakefile = do
    let files = [(Exe,".shake" </> "shake" <.> exe)
                ,(Haskell,"Shakefile.hs"),(Haskell,"Shakefile.lhs")
                ,(Make,"makefile"),(Make,"Makefile")
                ,(Ninja,"build.ninja")]
    res <- findM (fmap (either (const False) id) . try_ . IO.doesFileExist . snd) files
    case res of
        Just x -> return x
        Nothing -> do
            let Just (p1,p2) = unsnoc ["`" ++ x ++ "'" | (_,x) <- files]
            errorIO $ "Could not find " ++ intercalate ", " p1 ++ " or " ++ p2
