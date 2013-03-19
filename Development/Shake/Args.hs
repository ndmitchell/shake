
-- | Command line parsing flags.
module Development.Shake.Args(shakeOptDescrs, shakeArgs, shakeArgsWith) where

import Paths_shake
import Development.Shake.Types
import Development.Shake.Core
import Development.Shake.File
import Development.Shake.Progress
import Development.Shake.Shake

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Time
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit


-- | Run a build system using command line arguments for configuration.
--   The available flags are those from 'shakeOptDescrs', along with a few additional
--   @make@ compatible flags that are not represented in 'ShakeOptions', such as @--print-directory@.
--   If there are no file arguments then the 'Rules' are used directly, otherwise the file arguments
--   are 'want'ed (after calling 'withoutActions'). As an example:
--
-- @
-- main = 'shakeArgs' 'shakeOptions'{'shakeFiles' = \"_make/\", 'shakeProgress' = 'progressSimple'} $ do
--     'phony' \"clean\" $ 'Development.Shake.removeFilesAfter' \"_make\" [\"\/\/*\"]
--     'want' [\"_make\/neil.txt\",\"_make\/emily.txt\"]
--     \"_make\/*.txt\" '*>' \\out ->
--         ... build action here ...
-- @
--
--   This build system will default to building @neil.txt@ and @emily.txt@, while showing progress messages,
--   and putting the Shake files in locations such as @_make\/.database@. Some example command line flags:
--
-- * @main --no-progress@ will turn off progress messages.
--
-- * @main -j6@ will build on 6 threads.
--
-- * @main --help@ will display a list of supported flags.
--
-- * @main clean@ will not build anything, but will remove the @_make@ directory, including the
--   any 'shakeFiles'.
--
-- * @main _make/henry.txt@ will not build @neil.txt@ or @emily.txt@, but will instead build @henry.txt@.
shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs opts rules = shakeArgsWith opts [] f
    where f _ files = return $ Just $ if null files then rules else want files >> withoutActions rules


-- | A version of 'shakeArgs' with more flexible handling of command line arguments.
--   The caller of 'shakeArgsWith' can add additional flags (the second argument) and chose how to convert
--   the flags/arguments into rules (the third argument). Given:
--
-- @
-- 'shakeArgsWith' opts flags (\\flagValues argValues -> result)
-- @
--
-- * @opts@ is the initial 'ShakeOptions' value, which may have some fields overriden by command line flags.
--   This argument is usually 'shakeOptions', perhaps with a few fields overriden.
--
-- * @flags@ is a list of flag descriptions, which either produce a 'String' containing an error
--   message (typically for flags with invalid arguments, .e.g. @'Left' \"could not parse as int\"@), or a value
--   that is passed as @flagValues@. If you have no custom flags, pass @[]@.
--
-- * @flagValues@ is a list of custom flags that the user supplied. If @flags == []@ then this list will
--   be @[]@.
--
-- * @argValues@ is a list of non-flag arguments, which are often treated as files and passed to 'want'.
--
-- * @result@ should produce a 'Nothing' to indicate that no building needs to take place, or a 'Just'
--   providing the rules that should be used.
--
--   As an example of a build system that can use either @gcc@ or @distcc@ for compiling:
--
-- @
--import System.Console.GetOpt
--
--data Flags = DistCC deriving Eq
--flags = [Option \"\" [\"distcc\"] (NoArg $ Right DistCC) \"Run distributed.\"]
--
--main = 'shakeArgsWith' 'shakeOptions' flags $ \\flags targets -> return $ Just $ do
--     if null targets then 'want' [\"result.exe\"] else 'want' targets
--     let compiler = if DistCC \`elem\` flags then \"distcc\" else \"gcc\"
--     \"*.o\" '*>' \\out -> do
--         'need' ...
--         'Development.Shake.system'' compiler ...
--     ...
-- @
--
--   Now you can pass @--distcc@ to use the @distcc@ compiler.
shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith baseOpts userOptions rules = do
    args <- getArgs
    let (flags,files,errs) = getOpt Permute opts args
        (flagsError,flag1) = partitionEithers flags
        (self,user) = partitionEithers flag1
        (flagsExtra,flagsShake) = first concat $ unzip self
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
     else do
        when (Sleep `elem` flagsExtra) $ threadDelay 1000000
        start <- getCurrentTime
        curdir <- getCurrentDirectory
        let redir = case changeDirectory of
                Nothing -> id
                Just d -> bracket_ (setCurrentDirectory d) (setCurrentDirectory curdir)
        (ran,res) <- redir $ do
            when printDirectory $ putStrLn $ "shake: In directory `" ++ curdir ++ "'"
            rules <- rules user files
            case rules of
                Nothing -> return (False,Right ())
                Just rules -> do
                    res <- try $ shake shakeOpts rules
                    return (True, res)

        if not ran || shakeVerbosity shakeOpts < Normal || NoTime `elem` flagsExtra then
            either throwIO return res
         else
            let esc code = if Color `elem` flagsExtra then escape code else id
            in case res of
                Left err -> do
                    putStrLn $ esc "31" $ show (err :: SomeException)
                    exitFailure
                Right () -> do
                    stop <- getCurrentTime
                    let tot = diffUTCTime stop start
                        (mins,secs) = divMod (ceiling tot) (60 :: Int)
                        time = show mins ++ ":" ++ ['0' | secs < 10] ++ show secs
                    putStrLn $ esc "32" $ "Build completed in " ++ time ++ "m"
    where
        opts = map (wrap Left . snd) shakeOptsEx ++ map (wrap Right) userOptions
        showHelp = putStr $ unlines $ "Usage: shake [options] [target] ..." : "Options:" : showOptDescr opts

        wrap :: (a -> b) -> OptDescr (Either String a) -> OptDescr (Either String b)
        wrap f = fmapOptDescr (either Left (Right . f))


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
shakeOptDescrs = [fmapOptDescr (either Left (Right . snd)) o | (True, o) <- shakeOptsEx]

data Extra = ChangeDirectory FilePath
           | Version
           | AssumeNew FilePath
           | AssumeOld FilePath
           | PrintDirectory Bool
           | Color
           | Help
           | Sleep
           | NoTime
             deriving Eq


unescape :: String -> String
unescape ('\ESC':'[':xs) = unescape $ drop 1 $ dropWhile (not . isAlpha) xs
unescape (x:xs) = x : unescape xs
unescape [] = []

escape :: String -> String -> String
escape code x = "\ESC[" ++ code ++ "m" ++ x ++ "\ESC[0m"


-- | True if it has a potential effect on ShakeOptions
shakeOptsEx :: [(Bool, OptDescr (Either String ([Extra], ShakeOptions -> ShakeOptions)))]
shakeOptsEx =
    [yes $ Option "a" ["abbrev"] (pairArg "abbrev" "FULL=SHORT" $ \a s -> s{shakeAbbreviations=shakeAbbreviations s ++ [a]}) "Use abbreviation in status messages."
    ,yes $ Option "B" ["always-make"] (noArg $ \s -> s{shakeAssume=Just AssumeDirty}) "Unconditionally make all targets."
    ,no  $ Option "C" ["directory"] (ReqArg (\x -> Right ([ChangeDirectory x],id)) "DIRECTORY") "Change to DIRECTORY before doing anything."
    ,yes $ Option ""  ["color","colour"] (NoArg $ Right ([Color], \s -> s{shakeOutput=outputColor (shakeOutput s)})) "Colorize the output."
    ,yes $ Option "d" ["debug"] (OptArg (\x -> Right ([], \s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x})) "FILE") "Print lots of debugging information."
    ,yes $ Option ""  ["deterministic"] (noArg $ \s -> s{shakeDeterministic=True}) "Build rules in a fixed order."
    ,yes $ Option ""  ["flush"] (intArg "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
    ,yes $ Option ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    ,no  $ Option "h" ["help"] (NoArg $ Right ([Help],id)) "Print this message and exit."
    ,yes $ Option "j" ["jobs"] (intArg "jobs" "N" $ \i s -> s{shakeThreads=i}) "Allow N jobs/threads at once."
    ,yes $ Option "k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
    ,yes $ Option "l" ["lint"] (noArg $ \s -> s{shakeLint=True}) "Perform limited validation after the run."
    ,yes $ Option ""  ["no-lint"] (noArg $ \s -> s{shakeLint=False}) "Turn off --lint."
    ,yes $ Option "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ,no  $ Option "o" ["old-file","assume-old"] (ReqArg (\x -> Right ([AssumeOld x],id)) "FILE") "Consider FILE to be very old and don't remake it."
    ,yes $ Option ""  ["old-all"] (noArg $ \s -> s{shakeAssume=Just AssumeClean}) "Don't remake any files."
    ,yes $ Option "r" ["report"] (OptArg (\x -> Right ([], \s -> s{shakeReport=Just $ fromMaybe "report.html" x})) "FILE") "Write out profiling information [to report.html]."
    ,yes $ Option ""  ["no-report"] (noArg $ \s -> s{shakeReport=Nothing}) "Turn off --report."
    ,yes $ Option ""  ["rule-version"] (reqArg "VERSION" $ \x s -> s{shakeVersion=x}) "Version of the build rules."
    ,yes $ Option "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."
    ,no  $ Option ""  ["sleep"] (NoArg $ Right ([Sleep],id)) "Sleep for a second before building."
    ,yes $ Option "S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
    ,yes $ Option ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
    ,yes $ Option "p" ["progress"] (noArg $ \s -> s{shakeProgress=progressSimple}) "Show progress messages."
    ,yes $ Option " " ["no-progress"] (noArg $ \s -> s{shakeProgress=const $ return ()}) "Don't show progress messages."
    ,yes $ Option "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=Quiet}) "Don't print much."
    ,no  $ Option ""  ["no-time"] (NoArg $ Right ([NoTime],id)) "Don't print build time."
    ,yes $ Option "t" ["touch"] (noArg $ \s -> s{shakeAssume=Just AssumeClean}) "Assume targets are clean."
    ,yes $ Option "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=Loud}) "Print tracing information."
    ,no  $ Option "v" ["version"] (NoArg $ Right ([Version],id)) "Print the version number and exit."
    ,no  $ Option "w" ["print-directory"] (NoArg $ Right ([PrintDirectory True],id)) "Print the current directory."
    ,no  $ Option ""  ["no-print-directory"] (NoArg $ Right ([PrintDirectory False],id)) "Turn off -w, even if it was turned on implicitly."
    ,no  $ Option "W" ["what-if","new-file","assume-new"] (ReqArg (\x -> Right ([AssumeNew x],id)) "FILE") "Consider FILE to be infinitely new."
    ]
    where
        yes = (,) True
        no  = (,) False

        noArg f = NoArg $ Right ([], f)
        reqArg a f = ReqArg (\x -> Right ([], f x)) a
        intArg flag a f = flip ReqArg a $ \x -> case reads x of
            [(i,"")] | i >= 1 -> Right ([],f i)
            _ -> Left $ "the `--" ++ flag ++ "' option requires a positive integral argument"
        pairArg flag a f = flip ReqArg a $ \x -> case break (== '=') x of
            (a,'=':b) -> Right ([],f (a,b))
            _ -> Left $ "the `--" ++ flag ++ "' option requires an = in the argument"

        outputDebug output Nothing = output
        outputDebug output (Just file) = \v msg -> do
            when (v /= Diagnostic) $ output v msg
            appendFile file $ unescape msg

        outputColor output v msg = output v $ escape "34" msg
