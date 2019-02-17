{-# LANGUAGE TupleSections #-}

-- | Command line parsing flags.
module Development.Shake.Internal.Args(
    shakeOptDescrs,
    shake,
    shakeArgs, shakeArgsWith, shakeArgsOptionsWith
    ) where

import Development.Shake.Internal.Paths
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Errors
import Development.Shake.Internal.CompactUI
import Development.Shake.Internal.Demo
import Development.Shake.Internal.Core.Action
import Development.Shake.FilePath
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Progress
import Development.Shake.Database
import General.Timing
import General.Thread
import General.GetOpt

import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad
import Data.Char
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import System.Directory.Extra
import System.Environment
import System.Exit
import System.Time.Extra
import Prelude


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build. The function will throw
--   an exception if the build fails.
--
--   To use command line flags to modify 'ShakeOptions' see 'shakeArgs'.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
    addTiming "Function shake"
    (_, after) <- shakeWithDatabase opts rules $ \db -> do
        shakeOneShotDatabase db
        shakeRunDatabase db []
    shakeRunAfter opts after


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
--   If arguments are specified then typically the 'want' calls from the rules are discarded using 'withoutActions'.
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
--     let compiler = if DistCC \`elem\` flags then \"distcc\" else \"gcc\"
--     let rules = do
--         \"*.o\" '%>' \\out -> do
--             'need' ...
--             'cmd' compiler ...
--         'want' [\"target.exe\"]
--         ...
--     if null targets then rules else 'want' targets >> 'withoutActions' rules
-- @
--
--   Now you can pass @--distcc@ to use the @distcc@ compiler.
shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith opt args f = shakeArgsOptionsWith opt args $ \so a b -> fmap (so,) <$> f a b

-- | Like 'shakeArgsWith', but also lets you manipulate the 'ShakeOptions'.
shakeArgsOptionsWith
    :: ShakeOptions
    -> [OptDescr (Either String a)]
    -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ())))
    -> IO ()
shakeArgsOptionsWith baseOpts userOptions rules = do
    addTiming "shakeArgsWith"
    let baseOpts2 = removeOverlap userOptions $ map snd shakeOptsEx
    args <- getArgs
    let (flag1,files,errs) = getOpt (baseOpts2 `mergeOptDescr` userOptions) args
        (self,user) = partitionEithers flag1
        (flagsExtra,flagsShake) = first concat $ unzip self
        progressReplays = [x | ProgressReplay x <- flagsExtra]
        progressRecords = [x | ProgressRecord x <- flagsExtra]
        changeDirectory = listToMaybe [x | ChangeDirectory x <- flagsExtra]
        printDirectory = last $ False : [x | PrintDirectory x <- flagsExtra]
        shareRemoves = [x | ShareRemove x <- flagsExtra]
        oshakeOpts = foldl' (flip ($)) baseOpts flagsShake
        shakeOpts = oshakeOpts {shakeLintInside = map (toStandard . normalise . addTrailingPathSeparator) $
                                                  shakeLintInside oshakeOpts
                               ,shakeLintIgnore = map toStandard $
                                                  shakeLintIgnore oshakeOpts
                               ,shakeOutput     = if shakeColor oshakeOpts
                                                  then outputColor (shakeOutput oshakeOpts)
                                                  else shakeOutput oshakeOpts
                               }
    let putWhen v msg = when (shakeVerbosity oshakeOpts >= v) $ shakeOutput oshakeOpts v msg
    let putWhenLn v msg = putWhen v $ msg ++ "\n"
    let showHelp long = do
            progName <- getProgName
            extra <- if not long then return [] else do
                -- run the rules as simply as we can
                rs <- rules shakeOpts [] []
                case rs of
                    Just (_, rs) -> do
                        xs <- getTargets shakeOpts rs
                        return $ "" : "Targets:" : ["  - " ++ a ++ maybe "" (" - " ++) b | (a,b) <- xs]
                    _ -> return []

            putWhen Quiet $ unlines $
                ("Usage: " ++ progName ++ " [options] [target] ...") :
                (if null baseOpts2 then [] else "" : (if null userOptions then "Options:" else "Standard options:") : showOptDescr baseOpts2) ++
                (if null userOptions then [] else "" : "Extra options:" : showOptDescr userOptions) ++
                extra

    when (errs /= []) $ do
        putWhen Quiet $ unlines $ map ("shake: " ++) $ filter (not . null) $ lines $ unlines errs
        showHelp False
        exitFailure

    if Help `elem` flagsExtra then
        showHelp True
     else if Version `elem` flagsExtra then
        putWhenLn Normal $ "Shake build system, version " ++ shakeVersionString
     else if NumericVersion `elem` flagsExtra then
        putWhenLn Normal shakeVersionString
     else if Demo `elem` flagsExtra then
        demo $ shakeStaunch shakeOpts
     else if not $ null progressReplays then do
        dat <- forM progressReplays $ \file -> do
            src <- readFile file
            return (file, map read $ lines src)
        forM_ (if null $ shakeReport shakeOpts then ["-"] else shakeReport shakeOpts) $ \file -> do
            putWhenLn Normal $ "Writing report to " ++ file
            writeProgressReport file dat
     else do
        when (Sleep `elem` flagsExtra) $ sleep 1
        start <- offsetTime
        initDataDirectory -- must be done before we start changing directory
        let redir = maybe id withCurrentDirectory changeDirectory
        shakeOpts <- if null progressRecords then return shakeOpts else do
            t <- offsetTime
            return shakeOpts{shakeProgress = \p ->
                void $ withThreadsBoth (shakeProgress shakeOpts p) $
                    progressDisplay 1 (const $ return ()) $ do
                        p <- p
                        t <- t
                        forM_ progressRecords $ \file ->
                            appendFile file $ show (t,p) ++ "\n"
                        return p
            }
        (ran,shakeOpts,res) <- redir $ do
            when printDirectory $ do
                curdir <- getCurrentDirectory
                putWhenLn Normal $ "shake: In directory `" ++ curdir ++ "'"
            (shakeOpts, ui) <-
                if Compact `elem` flagsExtra
                then second withThreadSlave <$> compactUI shakeOpts
                else return (shakeOpts, id)
            rules <- rules shakeOpts user files
            ui $ case rules of
                Nothing -> return (False, shakeOpts, Right ())
                Just (shakeOpts, rules) -> do
                    res <- try_ $ shake shakeOpts $
                        if NoBuild `elem` flagsExtra then
                            withoutActions rules
                        else if ShareList `elem` flagsExtra || not (null shareRemoves) then do
                            action $ do
                                unless (null shareRemoves) $
                                    actionShareRemove shareRemoves
                                when (ShareList `elem` flagsExtra)
                                    actionShareList
                            withoutActions rules
                        else
                            rules
                    return (True, shakeOpts, res)

        if not ran || shakeVerbosity shakeOpts < Normal || NoTime `elem` flagsExtra then
            either throwIO return res
         else
            let esc = if shakeColor shakeOpts then escape else flip const
            in case res of
                Left err ->
                    if Exception `elem` flagsExtra then
                        throwIO err
                    else do
                        putWhenLn Quiet $ esc "31" $ show err
                        exitFailure
                Right () -> do
                    tot <- start
                    putWhenLn Normal $ esc "32" $ "Build completed in " ++ showDuration tot


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
           | Help
           | Sleep
           | NoTime
           | Exception
           | NoBuild
           | ProgressRecord FilePath
           | ProgressReplay FilePath
           | Demo
           | ShareList
           | ShareRemove String
           | Compact
             deriving Eq


unescape :: String -> String
unescape ('\ESC':'[':xs) = unescape $ drop 1 $ dropWhile (not . isAlpha) xs
unescape (x:xs) = x : unescape xs
unescape [] = []

escape :: String -> String -> String
escape code x = "\ESC[" ++ code ++ "m" ++ x ++ "\ESC[0m"

outputColor :: (Verbosity -> String -> IO ()) -> Verbosity -> String -> IO ()
outputColor output v msg = output v $ escape "34" msg

-- | True if it has a potential effect on ShakeOptions
shakeOptsEx :: [(Bool, OptDescr (Either String ([Extra], ShakeOptions -> ShakeOptions)))]
shakeOptsEx =
    [yes $ Option "a" ["abbrev"] (pairArg "abbrev" "FULL=SHORT" $ \a s -> s{shakeAbbreviations=shakeAbbreviations s ++ [a]}) "Use abbreviation in status messages."
    ,extr $ Option ""  ["no-build"] (NoArg $ Right [NoBuild]) "Don't build anything."
    ,extr $ Option "C" ["directory"] (ReqArg (\x -> Right [ChangeDirectory x]) "DIRECTORY") "Change to DIRECTORY before doing anything."
--    ,yes $ Option ""  ["cloud"] (reqArg "URL" $ \x s -> s{shakeCloud=shakeCloud s ++ [x]}) "HTTP server providing a cloud cache."
    ,yes $ Option ""  ["color","colour"] (noArg $ \s -> s{shakeColor=True}) "Colorize the output."
    ,yes $ Option ""  ["no-color","no-colour"] (noArg $ \s -> s{shakeColor=False}) "Don't colorize the output."
    ,extr $ Option ""  ["compact"] (NoArg $ Right [Compact]) "Use a compact Bazel/Buck style output."
    ,yes $ Option "d" ["debug"] (OptArg (\x -> Right ([], \s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x})) "FILE") "Print lots of debugging information."
    ,extr  $ Option ""  ["demo"] (NoArg $ Right [Demo]) "Run in demo mode."
    ,yes $ Option ""  ["digest"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeDigest})) "Files change when digest changes."
    ,yes $ Option ""  ["digest-and"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeAndDigest})) "Files change when modtime and digest change."
    ,yes $ Option ""  ["digest-and-input"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeAndDigestInput})) "Files change on modtime (and digest for inputs)."
    ,yes $ Option ""  ["digest-or"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtimeOrDigest})) "Files change when modtime or digest change."
    ,yes $ Option ""  ["digest-not"] (NoArg $ Right ([], \s -> s{shakeChange=ChangeModtime})) "Files change when modtime changes."
    ,extr $ Option ""  ["exception"] (NoArg $ Right [Exception]) "Throw exceptions directly."
    ,yes $ Option ""  ["flush"] (intArg 1 "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
    ,yes $ Option ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    ,extr $ Option "h" ["help"] (NoArg $ Right [Help]) "Print this message and exit."
    ,yes $ Option "j" ["jobs"] (optIntArg 0 "jobs" "N" $ \i s -> s{shakeThreads=fromMaybe 0 i}) "Allow N jobs/threads at once [default CPUs]."
    ,yes $ Option "k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
    ,yes $ Option "l" ["lint"] (noArg $ \s -> s{shakeLint=Just LintBasic}) "Perform limited validation after the run."
    ,yes $ Option ""  ["lint-watch"] (reqArg "PATTERN" $ \x s -> s{shakeLintWatch=shakeLintWatch s ++ [x]}) "Error if any of the patterns are created (expensive)."
    ,yes $ Option ""  ["lint-fsatrace"] (noArg $ \s -> s{shakeLint=Just LintFSATrace}) "Use fsatrace to do validation."
    ,yes $ Option ""  ["no-lint"] (noArg $ \s -> s{shakeLint=Nothing}) "Turn off --lint."
    ,yes $ Option ""  ["live"] (OptArg (\x -> Right ([], \s -> s{shakeLiveFiles=shakeLiveFiles s ++ [fromMaybe "live.txt" x]})) "FILE") "List the files that are live [to live.txt]."
    ,yes $ Option "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ,extr $ Option ""  ["numeric-version"] (NoArg $ Right [NumericVersion]) "Print just the version number and exit."
    ,yes $ Option ""  ["skip-commands"] (noArg $ \s -> s{shakeRunCommands=False}) "Try and avoid running external programs."
    ,yes $ Option ""  ["rebuild"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNow, fromMaybe "**" x)]})) "PATTERN") "Rebuild matching files."
    ,yes $ Option ""  ["no-rebuild"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNormal, fromMaybe "**" x)]})) "PATTERN") "Rebuild matching files if necessary (default)."
    ,yes $ Option ""  ["skip"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildLater, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files this run."
--    ,yes $ Option ""  ["skip-forever"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNever, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files until they change."
    ,yes $ Option "r" ["report","profile"] (OptArg (\x -> Right ([], \s -> s{shakeReport=shakeReport s ++ [fromMaybe "report.html" x]})) "FILE") "Write out profiling information [to report.html]."
    ,yes $ Option ""  ["no-reports"] (noArg $ \s -> s{shakeReport=[]}) "Turn off --report."
    ,yes $ Option ""  ["rule-version"] (reqArg "VERSION" $ \x s -> s{shakeVersion=x}) "Version of the build rules."
    ,yes $ Option ""  ["no-rule-version"] (noArg $ \s -> s{shakeVersionIgnore=True}) "Ignore the build rules version."
    ,yes $ Option ""  ["share"] (OptArg (\x -> Right ([], \s -> s{shakeShare=Just $ fromMaybe "" x, shakeChange=ensureHash $ shakeChange s})) "DIRECTORY") "Shared cache location."
    ,no  $ Option ""  ["share-list"] (NoArg $ Right ([ShareList], ensureShare)) "List the shared cache files."
    ,no  $ Option ""  ["share-remove"] (OptArg (\x -> Right ([ShareRemove $ fromMaybe "**" x], ensureShare)) "SUBSTRING") "Remove the shared cache keys."
    ,yes $ Option "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."
    ,extr $ Option ""  ["sleep"] (NoArg $ Right [Sleep]) "Sleep for a second before building."
    ,yes $ Option "S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
    ,yes $ Option ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
    ,yes $ Option "p" ["progress"] (progress $ optIntArg 1 "progress" "N" $ \i s -> s{shakeProgress=prog $ fromMaybe 5 i}) "Show progress messages [every N secs, default 5]."
    ,yes $ Option ""  ["no-progress"] (noArg $ \s -> s{shakeProgress=const $ return ()}) "Don't show progress messages."
    ,yes $ Option "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) pred}) "Print less (pass repeatedly for even less)."
    ,extr $ Option ""  ["no-time"] (NoArg $ Right [NoTime]) "Don't print build time."
    ,yes $ Option ""  ["timings"] (noArg $ \s -> s{shakeTimings=True}) "Print phase timings."
    ,yes $ Option "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) succ}) "Print more (pass repeatedly for even more)."
    ,extr $ Option "v" ["version"] (NoArg $ Right [Version]) "Print the version number and exit."
    ,extr $ Option "w" ["print-directory"] (NoArg $ Right [PrintDirectory True]) "Print the current directory."
    ,extr $ Option ""  ["no-print-directory"] (NoArg $ Right [PrintDirectory False]) "Turn off -w, even if it was turned on implicitly."
    ]
    where
        extr o = (False, fmapOptDescr (,id) o)

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
        progress _ = throwImpure $ errorInternal "incomplete pattern, progress"

        outputDebug output Nothing = output
        outputDebug output (Just file) = \v msg -> do
            when (v /= Diagnostic) $ output v msg
            appendFile file $ unescape msg ++ "\n"

        prog i p = do
            program <- progressProgram
            progressDisplay i (\s -> progressTitlebar s >> program s) p

        -- ensure the file system always computes a hash, required for --share
        ensureHash ChangeModtime = ChangeModtimeAndDigest
        ensureHash ChangeModtimeAndDigestInput = ChangeModtimeAndDigest
        ensureHash x = x

        ensureShare s = s{shakeShare = Just $ fromMaybe "." $ shakeShare s}
