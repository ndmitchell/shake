
module Run(main) where

import Development.Ninja.All
import System.Environment
import Development.Shake
import Development.Shake.FilePath
import General.Timing(resetTimings)
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Maybe
import Data.Char
import Data.String
import Data.List
import qualified System.Directory as IO
import System.Console.GetOpt
import System.Process
import System.Exit


main :: IO ()
main = do
    resetTimings
    args <- getArgs
    hsExe <- findFile
        [".shake" </> "shake" <.> exe
        ,"Shakefile.hs","Shakefile.lhs"]
    case hsExe of
        Just file -> do
            prog <- selectProg file
            e <- rawSystem prog args
            when (e /= ExitSuccess) $ exitWith e
        Nothing -> 
            withArgs ("--no-time":args) $
                shakeArgsWith shakeOptions{shakeCreationCheck=False} flags $ \opts targets -> do
                    let tool = listToMaybe [x | Tool x <- opts]
                    makefile <- case reverse [x | UseMakefile x <- opts] of
                        x:_ -> return x
                        _ -> do
                            res <- findFile ["build.ninja"]
                            case res of
                                Just x -> return x
                                Nothing -> errorIO "Could not find `build.ninja'"
                    runNinja makefile targets tool


data Flag = UseMakefile FilePath
          | Tool String

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."
        ,Option "t" ["tool"] (ReqArg (Right . Tool) "TOOL") "Ninja-compatible tools."
        ]

findFile :: [FilePath] -> IO (Maybe FilePath)
findFile = findM (fmap (either (const False) id) . try_ . IO.doesFileExist)

selectProg :: FilePath -> IO FilePath
selectProg file = if takeExtension file `elem` [".hs",".lhs"]
    then buildShakefile file
    else return $ toNative file

{-
buildShakefile :: FilePath -> IO FilePath
buildShakefile shakefile = do
    let shakefileBin = ".shake" </> shakefile -<.> exe
    () <- cmd ["ghc", "--make", shakefile, "-outputdir", ".shake"]
    return $ toNative shakefileBin
-}

buildShakefile :: FilePath -> IO FilePath
buildShakefile shakefile = do
    let shakefileBin = ".shake" </> shakefile -<.> exe
    () <- doBuild shakefile shakefileBin
    return $ toNative shakefileBin

doBuild :: FilePath -> FilePath -> IO ()
doBuild file target = shake shakeOptions { shakeFiles = ".shake" } $ do
    want [target]

    target %> \out -> do
        deps <- getDeps file
        need deps
        cmd ["ghc", "--make", file, "-o", target, "-outputdir", ".shake"]

getDeps :: FilePath -> Action [FilePath]
getDeps file = do
    () <- cmd ["ghc", "-M", "-dep-suffix=.", file]
    makefile <- liftIO $ readFile "Makefile"

    --parse the generated makefile into .hs dependencies
    let mkfLines = lines makefile
    let dropComments = map (takeWhile (\x -> x /= '#'))
    let dropEmpty = filter (\x -> length x > 0)
    let trimLeft = map (dropWhile isSpace)
    let trimRight = reverse . trimLeft . reverse
    let onlyHs = filter (isSuffixOf ".hs")
    let dropTarget = map ((drop 1) . (dropWhile (\x -> x /= ':')))
    let dropDups = map head . group . sort
    let parse = dropDups . trimLeft . dropTarget . onlyHs . trimRight . dropEmpty . dropComments
    return $ parse mkfLines
