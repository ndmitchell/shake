
module Run(main) where

import Development.Make.All
import Development.Ninja.All
import System.Environment
import Development.Shake
import Development.Shake.FilePath
import General.Timing
import Control.Applicative
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Maybe
import qualified System.Directory as IO
import System.Console.GetOpt
import System.Process
import System.Exit
import Prelude


main :: IO ()
main = do
    resetTimings
    args <- getArgs
    hsExe <- findFile
        [".shake" </> "shake" <.> exe
        ,"Shakefile.hs","Shakefile.lhs"]
    case hsExe of
        Just file -> do
            (prog,args) <- return $
                if takeExtension file `elem` [".hs",".lhs"] then ("runhaskell", file:args) else (toNative file, args)
            e <- rawSystem prog args
            when (e /= ExitSuccess) $ exitWith e
        Nothing -> 
            withArgs ("--no-time":args) $
                shakeArgsWith shakeOptions{shakeCreationCheck=False} flags $ \opts targets -> do
                    let tool = listToMaybe [x | Tool x <- opts]
                    makefile <- case reverse [x | UseMakefile x <- opts] of
                        x:_ -> return x
                        _ -> do
                            res <- findFile ["makefile","Makefile","build.ninja"]
                            case res of
                                Just x -> return x
                                Nothing -> errorIO "Could not find `makefile', `Makefile' or `build.ninja'"
                    case () of
                        _ | takeExtension makefile == ".ninja" -> runNinja makefile targets tool
                        _ | isJust tool -> error "--tool flag is not supported without a .ninja Makefile"
                        _ -> Just <$> runMakefile makefile targets


data Flag = UseMakefile FilePath
          | Tool String

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."
        ,Option "t" ["tool"] (ReqArg (Right . Tool) "TOOL") "Ninja-compatible tools."
        ]

findFile :: [FilePath] -> IO (Maybe FilePath)
findFile = findM (fmap (either (const False) id) . try_ . IO.doesFileExist)
