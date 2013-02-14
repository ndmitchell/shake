
-- | Command line parsing flags.
module Development.Shake.Args(shakeOptDescrs, shakeWithArgs) where

import Paths_shake
import Development.Shake.Types
import Development.Shake.Core
import Development.Shake.File
import Development.Shake.Progress
import Development.Shake.Shake

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit


-- | Run a Shake build system supporting basic @make@ compatible command line arguments.
--   Requires a way of cleaning the build objects (triggered by @clean@ as a target),
--   a base set of options that may be overriden by command line flags, and the set of build rules.
--   The function 'removeFiles' is often useful for producing a cleaning action.
--
--   The available command line options are those from 'shakeOptDescrs', along with a few additional
--   @make@ compatible flags that are not represented in 'ShakeOptions', such as @--print-directory@.
shakeWithArgs :: IO () -> ShakeOptions -> Rules () -> IO ()
shakeWithArgs clean baseOpts rules = do
    args <- getArgs
    let (flags,files,errs) = getOpt Permute opts args
        (flags1,flagsExtra) = partitionEithers flags
        (flagsError,flagsShake) = partitionEithers flags1
        assumeNew = [x | AssumeNew x <- flagsExtra]
        assumeOld = [x | AssumeOld x <- flagsExtra]
        changeDirectory = listToMaybe [x | ChangeDirectory x <- flagsExtra]
        printDirectory = last $ False : [x | PrintDirectory x <- flagsExtra]
        shakeOpts = foldl (flip ($)) baseOpts flagsShake

    -- error if you pass some clean and some dirty with specific flags
    errs <- return $ errs ++ flagsError ++ ["cannot mix " ++ a ++ " and " ++ b | a:b:_ <-
        [["`--assume-new'" | assumeNew/=[] ] ++ ["`--assume-old'" | assumeOld/=[] ] ++ ["explicit targets" | files/=[]]]]

    when (errs /= []) $ do
        putStr $ unlines $ map ("shake: " ++) $ filter (not . null) $ lines $ unlines errs
        showHelp
        exitFailure

    if Help `elem` flagsExtra then do
        showHelp
     else if Version `elem` flagsExtra then
        putStrLn $ "Shake build system, version " ++ show version
     else if "clean" `elem` files then
        clean
     else do
        when (Clean `elem` flagsExtra) clean
        when (Sleep `elem` flagsExtra) $ threadDelay 1000000
        curdir <- getCurrentDirectory
        let redir = case changeDirectory of
                Nothing -> id
                Just d -> bracket_ (setCurrentDirectory d) (setCurrentDirectory curdir)
        redir $ do
            when printDirectory $ putStrLn $ "shake: In directory `" ++ curdir ++ "'"
            shake shakeOpts $ if null files then rules else want files >> withoutActions rules
    where
        opts = map (either (fmapOptDescr Left) (fmapOptDescr Right)) shakeOptsEx
        showHelp = putStr $ unlines $ "Usage: shake [options] [target] ..." : "Options:" : showOptDescr opts


showOptDescr :: [OptDescr a] -> [String]
showOptDescr xs = concat
    [ if nargs <= 26 then ["  " ++ args ++ replicate (28 - nargs) ' ' ++ desc]
                     else ["  " ++ args, replicate 30 ' ' ++ desc]
    | Option s l arg desc <- xs
    , let args = intercalate ", " $ map (short arg) s ++ map (long arg) l
    , let nargs = length args]
    where short NoArg{} x = "-" ++ [x]
          short (ReqArg _ b) x = "-" ++ [x] ++ " " ++ b
          short (OptArg _ b) x = "-" ++ [x] ++ "[=" ++ b ++ "]"
          long NoArg{} x = "--" ++ x
          long (ReqArg _ b) x = "--" ++ x ++ "=" ++ b
          long (OptArg _ b) x = "--" ++ x ++ "[=" ++ b ++ "]"


fmapOptDescr :: (a -> b) -> OptDescr a -> OptDescr b
fmapOptDescr f (Option a b c d) = Option a b (g c) d
    where g (NoArg a) = NoArg $ f a
          g (ReqArg a b) = ReqArg (f . a) b
          g (OptArg a b) = OptArg (f . a) b


-- | A list of command line options that can be used to modify 'ShakeOptions'. Each option returns
--   either an error message (invalid argument to the flag) or a function that changes some fields
--   in 'ShakeOptions'. The command line flags are @make@ compatible where possbile, but additional
--   flags have been added for the extra options Shake supports.
shakeOptDescrs :: [OptDescr (Either String (ShakeOptions -> ShakeOptions))]
shakeOptDescrs = lefts shakeOptsEx

data Extra = ChangeDirectory FilePath
           | Version
           | AssumeNew FilePath
           | AssumeOld FilePath
           | PrintDirectory Bool
           | Help
           | Clean
           | Sleep
             deriving Eq


shakeOptsEx :: [Either (OptDescr (Either String (ShakeOptions -> ShakeOptions))) (OptDescr Extra)]
shakeOptsEx =
-- Options not in make --help are prefixed with {--}, rest 
{--}[Left  $ Option "a" ["abbrev"] (pairArg "abbrev" "FULL=SHORT" $ \a s -> s{shakeAbbreviations=shakeAbbreviations s ++ [a]}) "Use abbreviation in status messages."
    ,Left  $ Option "B" ["always-make"] (noArg $ \s -> s{shakeAssume=Just AssumeDirty}) "Unconditionally make all targets."
{--},Right $ Option "c" ["clean"] (NoArg Clean) "Clean before building."
    ,Right $ Option "C" ["directory"] (ReqArg ChangeDirectory "DIRECTORY") "Change to DIRECTORY before doing anything."
    ,Left  $ Option "d" ["debug"] (OptArg (\x -> Right $ \s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x}) "FILE") "Print lots of debugging information."
{--},Left  $ Option ""  ["deterministic"] (noArg $ \s -> s{shakeDeterministic=True}) "Build rules in a fixed order."
{--},Left  $ Option "f" ["flush"] (intArg "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
{--},Left  $ Option ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    ,Right $ Option "h" ["help"] (NoArg Help) "Print this message and exit."
    ,Left  $ Option "j" ["jobs"] (intArg "jobs" "N" $ \i s -> s{shakeThreads=i}) "Allow N jobs/threads at once."
    ,Left  $ Option "k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
{--},Left  $ Option "l" ["lint"] (noArg $ \s -> s{shakeLint=True}) "Perform limited validation after the run."
{--},Left  $ Option "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ,Right $ Option "o" ["old-file","assume-old"] (ReqArg AssumeOld "FILE") "Consider FILE to be very old and don't remake it."
{--},Left  $ Option ""  ["old-all"] (noArg $ \s -> s{shakeAssume=Just AssumeClean}) "Don't remake any files."
{--},Left  $ Option "r" ["report"] (OptArg (\x -> Right $ \s -> s{shakeReport=Just $ fromMaybe "report.html" x}) "FILE") "Write out profiling information [to report.html]."
{--},Left  $ Option ""  ["rule-version"] (intArg "rule-version" "N" $ \x s -> s{shakeVersion=x}) "Version number of the build rules."
    ,Left  $ Option "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."
{--},Right $ Option ""  ["sleep"] (NoArg Sleep) "Sleep for a second before building."
    ,Left  $ Option "S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
{--},Left  $ Option ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
{--},Left  $ Option "p" ["progress"] (noArg $ \s -> s{shakeProgress=progressSimple}) "Show progress messages."
    ,Left  $ Option "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=Quiet}) "Don't print much."
    ,Left  $ Option "t" ["touch"] (noArg $ \s -> s{shakeAssume=Just AssumeClean}) "Assume targets are clean."
    ,Left  $ Option "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=Loud}) "Print tracing information."
    ,Right $ Option "v" ["version"] (NoArg Version) "Print the version number and exit."
    ,Right $ Option "w" ["print-directory"] (NoArg $ PrintDirectory True) "Print the current directory."
    ,Right $ Option ""  ["no-print-directory"] (NoArg $ PrintDirectory False) "Turn off -w, even if it was turned on implicitly."
    ,Right $ Option "W" ["what-if","new-file","assume-new"] (ReqArg AssumeNew "FILE") "Consider FILE to be infinitely new."
    ]
    where
        noArg f = NoArg $ Right f
        reqArg a f = ReqArg (Right . f) a
        intArg flag a f = flip ReqArg a $ \x -> case reads x of
            [(i,"")] | i >= 1 -> Right $ f i
            _ -> Left $ "the `--" ++ flag ++ "' option requires a positive integral argument"
        pairArg flag a f = flip ReqArg a $ \x -> case break (== '=') x of
            (a,'=':b) -> Right $ f (a,b)
            _ -> Left $ "the `--" ++ flag ++ "' option requires an = in the argument"

        outputDebug output Nothing = output
        outputDebug output (Just file) = \v msg -> if v == Diagnostic then appendFile file msg else output v file
