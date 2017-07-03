
-- | Command line parsing flags.
module Development.Shake.Internal.Args(shakeOptDescrs, shakeArgs, shakeArgsWith) where

import Paths_shake
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Demo
import Development.Shake.FilePath
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Progress
import Development.Shake.Internal.Shake
import General.Timing
import General.GetOpt

import Data.Tuple.Extra
import Control.Concurrent
import Control.Exception.Extra
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Version(showVersion)
import System.Directory
import System.Environment
import System.Exit
import System.Time.Extra


-- | Run a build system using command line arguments for configuration.
--   The available flags are those from 'shakeOptDescrs', along with a few additional
--   @make@ compatible flags that are not represented in 'ShakeOptions', such as @--print-directory@.
--   If there are no file arguments then the 'Rules' are used directly, otherwise the file arguments
--   are 'want'ed (after calling 'withoutActions'). As an example:
--
-- @
-- main = 'shakeArgs' 'shakeOptions'{'shakeFiles' = \"_make\", 'shakeProgress' = 'progressSimple'} $ do
--     'phony' \"clean\" $ 'Development.Shake.removeFilesAfter' \"_make\" [\"\/\/*\"]
--     'want' [\"_make\/neil.txt\",\"_make\/emily.txt\"]
--     \"_make\/*.txt\" '%>' \\out ->
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
-- import System.Console.GetOpt
--
-- data Flags = DistCC deriving Eq
-- flags = [Option \"\" [\"distcc\"] (NoArg $ Right DistCC) \"Run distributed.\"]
--
-- main = 'shakeArgsWith' 'shakeOptions' flags $ \\flags targets -> return $ Just $ do
--     if null targets then 'want' [\"result.exe\"] else 'want' targets
--     let compiler = if DistCC \`elem\` flags then \"distcc\" else \"gcc\"
--     \"*.o\" '%>' \\out -> do
--         'need' ...
--         'cmd' compiler ...
--     ...
-- @
--
--   Now you can pass @--distcc@ to use the @distcc@ compiler.
shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith baseOpts userOptions rules = do
    addTiming "shakeArgsWith"
    args <- getArgs
    let (flag1,files,errs) = getOpt opts args
        (self,user) = partitionEithers flag1
        (flagsExtra,flagsShake) = first concat $ unzip self
        progressReplays = [x | ProgressReplay x <- flagsExtra]
        progressRecords = [x | ProgressRecord x <- flagsExtra]
        changeDirectory = listToMaybe [x | ChangeDirectory x <- flagsExtra]
        printDirectory = last $ False : [x | PrintDirectory x <- flagsExtra]
        oshakeOpts = foldl' (flip ($)) baseOpts flagsShake
        shakeOpts = oshakeOpts {shakeLintInside = map (toStandard . normalise . addTrailingPathSeparator) $
                                                  shakeLintInside oshakeOpts
                               ,shakeLintIgnore = map toStandard $
                                                  shakeLintIgnore oshakeOpts
                               }

    when (errs /= []) $ do
        putStr $ unlines $ map ("shake: " ++) $ filter (not . null) $ lines $ unlines errs
        showHelp
        exitFailure

    if Help `elem` flagsExtra then
        showHelp
     else if Version `elem` flagsExtra then
        putStrLn $ "Shake build system, version " ++ showVersion version
     else if NumericVersion `elem` flagsExtra then
        putStrLn $ showVersion version
     else if Demo `elem` flagsExtra then
        demo $ shakeStaunch shakeOpts
     else if not $ null progressReplays then do
        dat <- forM progressReplays $ \file -> do
            src <- readFile file
            return (file, map read $ lines src)
        forM_ (if null $ shakeReport shakeOpts then ["-"] else shakeReport shakeOpts) $ \file -> do
            putStrLn $ "Writing report to " ++ file
            writeProgressReport file dat
     else do
        when (Sleep `elem` flagsExtra) $ threadDelay 1000000
        start <- offsetTime
        curdir <- getCurrentDirectory
        let redir = case changeDirectory of
                Nothing -> id
                -- get the "html" directory so it caches with the current directory
                -- required only for debug code
                Just d -> bracket_ (getDataFileName "html" >> setCurrentDirectory d) (setCurrentDirectory curdir)
        shakeOpts <- if null progressRecords then return shakeOpts else do
            t <- offsetTime
            return shakeOpts{shakeProgress = \p ->
                bracket
                    (forkIO $ shakeProgress shakeOpts p)
                    killThread
                    $ const $ progressDisplay 1 (const $ return ()) $ do
                        p <- p
                        t <- t
                        forM_ progressRecords $ \file ->
                            appendFile file $ show (t,p) ++ "\n"
                        return p
            }
        (ran,res) <- redir $ do
            when printDirectory $ putStrLn $ "shake: In directory `" ++ curdir ++ "'"
            rules <- rules user files
            case rules of
                Nothing -> return (False,Right ())
                Just rules -> do
                    res <- try_ $ shake shakeOpts $
                        if NoBuild `elem` flagsExtra then withoutActions rules else rules
                    return (True, res)

        if not ran || shakeVerbosity shakeOpts < Normal || NoTime `elem` flagsExtra then
            either throwIO return res
         else
            let esc code = if Color `elem` flagsExtra then escape code else id
            in case res of
                Left err ->
                    if Exception `elem` flagsExtra then
                        throwIO err
                    else do
                        putStrLn $ esc "31" $ show err
                        exitFailure
                Right () -> do
                    tot <- start
                    let (mins,secs) = divMod (ceiling tot) (60 :: Int)
                        time = show mins ++ ":" ++ ['0' | secs < 10] ++ show secs
                    putStrLn $ esc "32" $ "Build completed in " ++ time ++ "m"
    where
        opts = removeOverlap userOptions (map snd shakeOptsEx) `mergeOptDescr` userOptions
        showHelp = do
            progName <- getProgName
            putStr $ unlines $ ("Usage: " ++ progName ++ " [options] [target] ...") : "Options:" : showOptDescr opts


-- | A list of command line options that can be used to modify 'ShakeOptions'. Each option returns
--   either an error message (invalid argument to the flag) or a function that changes some fields
--   in 'ShakeOptions'. The command line flags are @make@ compatible where possbile, but additional
--   flags have been added for the extra options Shake supports.
shakeOptDescrs :: [OptDescr (Either String (ShakeOptions -> ShakeOptions))]
shakeOptDescrs = [fmapOptDescr snd o | (True, o) <- shakeOptsEx]

data Extra = ChangeDirectory FilePath
           | Version
           | NumericVersion
           | PrintDirectory Bool
           | Color
           | Help
           | Sleep
           | NoTime
           | Exception
           | NoBuild
           | ProgressRecord FilePath
           | ProgressReplay FilePath
           | Demo
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
    ,no  $ Option ""  ["no-build"] (NoArg $ Right ([NoBuild], id)) "Don't build anything."
    ,no  $ Option "C" ["directory"] (ReqArg (\x -> Right ([ChangeDirectory x],id)) "DIRECTORY") "Change to DIRECTORY before doing anything."
    ,yes $ Option ""  ["color","colour"] (NoArg $ Right ([Color], \s -> s{shakeOutput=outputColor (shakeOutput s)})) "Colorize the output."
    ,yes $ Option "d" ["debug"] (OptArg (\x -> Right ([], \s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x})) "FILE") "Print lots of debugging information."
    ,no  $ Option ""  ["demo"] (NoArg $ Right ([Demo], id)) "Run in demo mode."
    ,yes $ Option ""  ["digest"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeDigest})) "Files change when digest changes."
    ,yes $ Option ""  ["digest-and"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeAndDigest})) "Files change when modtime and digest change."
    ,yes $ Option ""  ["digest-and-input"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeAndDigestInput})) "Files change on modtime (and digest for inputs)."
    ,yes $ Option ""  ["digest-or"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeOrDigest})) "Files change when modtime or digest change."
    ,yes $ Option ""  ["digest-not"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtime})) "Files change when modtime changes."
    ,no  $ Option ""  ["exception"] (NoArg $ Right ([Exception], id)) "Throw exceptions directly."
    ,yes $ Option ""  ["flush"] (intArg 1 "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
    ,yes $ Option ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    ,no  $ Option "h" ["help"] (NoArg $ Right ([Help],id)) "Print this message and exit."
    ,yes $ Option "j" ["jobs"] (optIntArg 0 "jobs" "N" $ \i s -> s{shakeThreads=fromMaybe 0 i}) "Allow N jobs/threads at once [default CPUs]."
    ,yes $ Option "k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
    ,yes $ Option "l" ["lint"] (noArg $ \s -> s{shakeLint=Just LintBasic}) "Perform limited validation after the run."
    ,yes $ Option ""  ["lint-fsatrace"] (noArg $ \s -> s{shakeLint=Just LintFSATrace}) "Use fsatrace to do validation."
    ,yes $ Option ""  ["no-lint"] (noArg $ \s -> s{shakeLint=Nothing}) "Turn off --lint."
    ,yes $ Option ""  ["live"] (OptArg (\x -> Right ([], \s -> s{shakeLiveFiles=shakeLiveFiles s ++ [fromMaybe "live.txt" x]})) "FILE") "List the files that are live [to live.txt]."
    ,yes $ Option "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ,no  $ Option ""  ["numeric-version"] (NoArg $ Right ([NumericVersion],id)) "Print just the version number and exit."
    ,yes $ Option ""  ["skip-commands"] (noArg $ \s -> s{shakeRunCommands=False}) "Try and avoid running external programs."
    ,yes $ Option ""  ["rebuild"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNow, fromMaybe "**" x)]})) "PATTERN") "Rebuild matching files."
    ,yes $ Option ""  ["no-rebuild"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNormal, fromMaybe "**" x)]})) "PATTERN") "Rebuild matching files if necessary."
    ,yes $ Option ""  ["skip"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildLater, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files this run."
    ,yes $ Option ""  ["skip-forever"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNever, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files until they change."
    ,yes $ Option "r" ["report","profile"] (OptArg (\x -> Right ([], \s -> s{shakeReport=shakeReport s ++ [fromMaybe "report.html" x]})) "FILE") "Write out profiling information [to report.html]."
    ,yes $ Option ""  ["no-reports"] (noArg $ \s -> s{shakeReport=[]}) "Turn off --report."
    ,yes $ Option ""  ["rule-version"] (reqArg "VERSION" $ \x s -> s{shakeVersion=x}) "Version of the build rules."
    ,yes $ Option ""  ["no-rule-version"] (noArg $ \s -> s{shakeVersionIgnore=True}) "Ignore the build rules version."
    ,yes $ Option "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."
    ,no  $ Option ""  ["sleep"] (NoArg $ Right ([Sleep],id)) "Sleep for a second before building."
    ,yes $ Option "S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
    ,yes $ Option ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
    ,yes $ Option "p" ["progress"] (progress $ optIntArg 1 "progress" "N" $ \i s -> s{shakeProgress=prog $ fromMaybe 5 i}) "Show progress messages [every N secs, default 5]."
    ,yes $ Option ""  ["no-progress"] (noArg $ \s -> s{shakeProgress=const $ return ()}) "Don't show progress messages."
    ,yes $ Option "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) pred}) "Don't print much."
    ,no  $ Option ""  ["no-time"] (NoArg $ Right ([NoTime],id)) "Don't print build time."
    ,yes $ Option ""  ["timings"] (noArg $ \s -> s{shakeTimings=True}) "Print phase timings."
    ,yes $ Option "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) succ}) "Print tracing information."
    ,no  $ Option "v" ["version"] (NoArg $ Right ([Version],id)) "Print the version number and exit."
    ,no  $ Option "w" ["print-directory"] (NoArg $ Right ([PrintDirectory True],id)) "Print the current directory."
    ,no  $ Option ""  ["no-print-directory"] (NoArg $ Right ([PrintDirectory False],id)) "Turn off -w, even if it was turned on implicitly."
    ]
    where
        yes = (,) True
        no  = (,) False

        move :: Verbosity -> (Int -> Int) -> Verbosity
        move x by = toEnum $ min (fromEnum mx) $ max (fromEnum mn) $ by $ fromEnum x
            where (mn,mx) = (asTypeOf minBound x, asTypeOf maxBound x)

        noArg f = NoArg $ Right ([], f)
        reqArg a f = ReqArg (\x -> Right ([], f x)) a
        intArg mn flag a f = flip ReqArg a $ \x -> case reads x of
            [(i,"")] | i >= mn -> Right ([],f i)
            _ -> Left $ "the `--" ++ flag ++ "' option requires a number, " ++ show mn ++ " or above"
        optIntArg mn flag a f = flip OptArg a $ maybe (Right ([], f Nothing)) $ \x -> case reads x of
            [(i,"")] | i >= mn -> Right ([],f $ Just i)
            _ -> Left $ "the `--" ++ flag ++ "' option requires a number, " ++ show mn ++ " or above"
        pairArg flag a f = flip ReqArg a $ \x -> case break (== '=') x of
            (a,'=':b) -> Right ([],f (a,b))
            _ -> Left $ "the `--" ++ flag ++ "' option requires an = in the argument"

        progress (OptArg func msg) = flip OptArg msg $ \x -> case break (== '=') `fmap` x of
            Just ("record",file) -> Right ([ProgressRecord $ if null file then "progress.txt" else tail file], id)
            Just ("replay",file) -> Right ([ProgressReplay $ if null file then "progress.txt" else tail file], id)
            _ -> func x

        outputDebug output Nothing = output
        outputDebug output (Just file) = \v msg -> do
            when (v /= Diagnostic) $ output v msg
            appendFile file $ unescape msg ++ "\n"

        outputColor output v msg = output v $ escape "34" msg

        prog i p = do
            program <- progressProgram
            progressDisplay i (\s -> progressTitlebar s >> program s) p
