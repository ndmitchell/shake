{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeOperators, ScopedTypeVariables, NamedFieldPuns #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif


-- | This module provides functions for calling command line programs, primarily
--   'command' and 'cmd'. As a simple example:
--
-- @
-- 'command' [] \"gcc\" [\"-c\",myfile]
-- @
--
--   The functions from this module are now available directly from "Development.Shake".
--   You should only need to import this module if you are using the 'cmd' function in the 'IO' monad.
module Development.Shake.Command(
    command, command_, cmd, cmd_, unit, CmdArgument(..), CmdArguments(..), IsCmdArgument(..), (:->),
    Stdout(..), Stderr(..), Stdouterr(..), Exit(..), Process(..), CmdTime(..), CmdLine(..),
    CmdResult, CmdString, CmdOption(..),
    addPath, addEnv,
    ) where

import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Char
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import Data.Monoid
import System.Directory
import System.Environment.Extra
import System.Exit
import System.IO.Extra hiding (withTempFile, withTempDir)
import System.Process
import System.Info.Extra
import System.Time.Extra
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import General.Process
import Control.Applicative
import Prelude

import Development.Shake.Internal.CmdOption
import Development.Shake.Internal.Core.Run
import Development.Shake.FilePath
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.Options
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Derived

---------------------------------------------------------------------
-- ACTUAL EXECUTION

-- | /Deprecated:/ Use 'AddPath'. This function will be removed in a future version.
--
--   Add a prefix and suffix to the @$PATH@ environment variable. For example:
--
-- @
-- opt <- 'addPath' [\"\/usr\/special\"] []
-- 'cmd' opt \"userbinary --version\"
-- @
--
--   Would prepend @\/usr\/special@ to the current @$PATH@, and the command would pick
--   @\/usr\/special\/userbinary@, if it exists. To add other variables see 'addEnv'.
addPath :: MonadIO m => [String] -> [String] -> m CmdOption
addPath pre post = do
    args <- liftIO getEnvironment
    let (path,other) = partition ((== "PATH") . (if isWindows then upper else id) . fst) args
    return $ Env $
        [("PATH",intercalate [searchPathSeparator] $ pre ++ post) | null path] ++
        [(a,intercalate [searchPathSeparator] $ pre ++ [b | b /= ""] ++ post) | (a,b) <- path] ++
        other

-- | /Deprecated:/ Use 'AddEnv'. This function will be removed in a future version.
--
--   Add a single variable to the environment. For example:
--
-- @
-- opt <- 'addEnv' [(\"CFLAGS\",\"-O2\")]
-- 'cmd' opt \"gcc -c main.c\"
-- @
--
--   Would add the environment variable @$CFLAGS@ with value @-O2@. If the variable @$CFLAGS@
--   was already defined it would be overwritten. If you wish to modify @$PATH@ see 'addPath'.
addEnv :: MonadIO m => [(String, String)] -> m CmdOption
addEnv extra = do
    args <- liftIO getEnvironment
    return $ Env $ extra ++ filter (\(a,_) -> a `notElem` map fst extra) args


data Str = Str String | BS BS.ByteString | LBS LBS.ByteString | Unit deriving Eq

data Result
    = ResultStdout Str
    | ResultStderr Str
    | ResultStdouterr Str
    | ResultCode ExitCode
    | ResultTime Double
    | ResultLine String
    | ResultProcess Pid
      deriving Eq

data Pid = Pid0 | Pid ProcessHandle
instance Eq Pid where _ == _ = True


---------------------------------------------------------------------
-- ACTION EXPLICIT OPERATION

-- | Given explicit operations, apply the advance ones, like skip/trace/track/autodep
commandExplicit :: String -> [CmdOption] -> [Result] -> String -> [String] -> Action [Result]
commandExplicit funcName oopts results exe args = do
    ShakeOptions
        {shakeCommandOptions,shakeRunCommands
        ,shakeLint,shakeLintInside,shakeLintIgnore} <- getShakeOptions
    let fopts = shakeCommandOptions ++ oopts
    let useShell = Shell `elem` fopts
    let useLint = shakeLint == Just LintFSATrace
    let useAutoDeps = AutoDeps `elem` fopts
    let opts = filter (/= Shell) fopts

    let skipper act = if null results && not shakeRunCommands then return [] else act

    let verboser act = do
            let cwd = listToMaybe $ reverse [x | Cwd x <- opts]
            putLoud $ maybe "" (\x -> "cd " ++ x ++ "; ") cwd ++ showCommandForUser2 exe args
            verb <- getVerbosity
            (if verb >= Loud then quietly else id) act

    let tracer = case reverse [x | Traced x <- opts] of
            "":_ -> liftIO
            msg:_ -> traced msg
            [] -> traced (takeFileName exe)

    let tracker act
            | useLint = fsatrace act
            | useAutoDeps = autodeps act
            | useShell = shelled act
            | otherwise = act exe args

        shelled = runShell (unwords $ exe : args)

        ignore = map (?==) shakeLintIgnore
        ham cwd xs = [makeRelative cwd x | x <- map toStandard xs
                                         , any (`isPrefixOf` x) shakeLintInside
                                         , not $ any ($ x) ignore]

        fsaCmd act opts file
            | isMac = fsaCmdMac act opts file
            | useShell = runShell (unwords $ exe : args) $ \exe args -> act "fsatrace" $ opts : file : "--" : exe : args
            | otherwise = act "fsatrace" $ opts : file : "--" : exe : args

        fsaCmdMac act opts file = do
            let fakeExe e = liftIO $ do
                    me <- findExecutable e
                    case me of
                        Just re -> do
                            let isSystem = any (`isPrefixOf` re) [ "/bin"
                                                                 , "/usr"
                                                                 , "/sbin"
                                                                 ]
                            if isSystem
                                then do
                                    tmpdir <- getTemporaryDirectory
                                    let fake = tmpdir ++ "fsatrace-fakes" ++ re
                                    unlessM (doesFileExist fake) $ do
                                        createDirectoryIfMissing True $ takeDirectory fake
                                        copyFile re fake
                                    return fake
                                else return re
                        Nothing -> return e
            fexe <- fakeExe exe
            if useShell
                then do
                    fsh <- fakeExe "/bin/sh"
                    act "fsatrace" $ opts : file : "--" : fsh : "-c" : [unwords $ fexe : args]
                else act "fsatrace" $ opts : file : "--" : fexe : args

        fsatrace act = withTempFile $ \file -> do
            res <- fsaCmd act "rwm" file
            xs <- liftIO $ parseFSAT <$> readFileUTF8' file
            cwd <- liftIO getCurrentDirectory
            let reader (FSATRead x) = Just x; reader _ = Nothing
                writer (FSATWrite x) = Just x; writer (FSATMove x _) = Just x; writer _ = Nothing
                existing f = liftIO . filterM doesFileExist . nubOrd . mapMaybe f
            rs <- existing reader xs
            ws <- existing writer xs
            let reads = ham cwd rs
                writes = ham cwd ws
            when useAutoDeps $
                unsafeAllowApply $ needed reads
            trackRead reads
            trackWrite writes
            return res

        autodeps act = withTempFile $ \file -> do
            res <-  fsaCmd act "r" file
            pxs <- liftIO $ parseFSAT <$> readFileUTF8' file
            xs <- liftIO $ filterM doesFileExist [x | FSATRead x <- pxs]
            cwd <- liftIO getCurrentDirectory
            unsafeAllowApply $ need $ ham cwd xs
            return res

    skipper $ tracker $ \exe args -> verboser $ tracer $ commandExplicitIO funcName opts results exe args


-- | Given a shell command, call the continuation with the sanitised exec-style arguments
runShell :: String -> (String -> [String] -> Action a) -> Action a
runShell x act | not isWindows = act "/bin/sh" ["-c",x] -- do exactly what Haskell does
runShell x act = withTempDir $ \dir -> do
    let file = dir </> "s.bat"
    writeFile' file x
    act "cmd.exe" ["/d/q/c",file]


-- | Parse the FSATrace structure
data FSAT
    = FSATWrite FilePath
    | FSATRead FilePath
    | FSATDelete FilePath
    | FSATMove FilePath FilePath

-- | Parse the 'FSAT' entries, ignoring anything you don't understand.
parseFSAT :: String -> [FSAT]
parseFSAT = mapMaybe f . lines
    where f ('w':'|':xs) = Just $ FSATWrite xs
          f ('r':'|':xs) = Just $ FSATRead xs
          f ('d':'|':xs) = Just $ FSATDelete xs
          f ('m':'|':xs) | (xs,'|':ys) <- break (== '|') xs = Just $ FSATMove xs ys
          f _ = Nothing

---------------------------------------------------------------------
-- IO EXPLICIT OPERATION

-- | Given a very explicit set of CmdOption, translate them to a General.Process structure
commandExplicitIO :: String -> [CmdOption] -> [Result] -> String -> [String] -> IO [Result]
commandExplicitIO funcName opts results exe args = do
    let (grabStdout, grabStderr) = both or $ unzip $ for results $ \r -> case r of
            ResultStdout{} -> (True, False)
            ResultStderr{} -> (False, True)
            ResultStdouterr{} -> (True, True)
            _ -> (False, False)

    optEnv <- resolveEnv opts
    let optCwd = let x = last $ "" : [x | Cwd x <- opts] in if x == "" then Nothing else Just x
    let optStdin = flip mapMaybe opts $ \x -> case x of
            Stdin x -> Just $ SrcString x
            StdinBS x -> Just $ SrcBytes x
            FileStdin x -> Just $ SrcFile x
            _ -> Nothing
    let optShell = Shell `elem` opts
    let optBinary = BinaryPipes `elem` opts
    let optAsync = ResultProcess Pid0 `elem` results
    let optTimeout = listToMaybe $ reverse [x | Timeout x <- opts]
    let optWithStdout = last $ False : [x | WithStdout x <- opts]
    let optWithStderr = last $ True : [x | WithStderr x <- opts]
    let optFileStdout = [x | FileStdout x <- opts]
    let optFileStderr = [x | FileStderr x <- opts]
    let optEchoStdout = last $ (not grabStdout && null optFileStdout) : [x | EchoStdout x <- opts]
    let optEchoStderr = last $ (not grabStderr && null optFileStderr) : [x | EchoStderr x <- opts]

    let cmdline = showCommandForUser2 exe args
    let bufLBS f = do (a,b) <- buf $ LBS LBS.empty; return (a, (\(LBS x) -> f x) <$> b)
        buf Str{} | optBinary = bufLBS (Str . LBS.unpack)
        buf Str{} = do x <- newBuffer; return ([DestString x | not optAsync], Str . concat <$> readBuffer x)
        buf LBS{} = do x <- newBuffer; return ([DestBytes x | not optAsync], LBS . LBS.fromChunks <$> readBuffer x)
        buf BS {} = bufLBS (BS . BS.concat . LBS.toChunks)
        buf Unit  = return ([], return Unit)
    (dStdout, dStderr, resultBuild) :: ([[Destination]], [[Destination]], [Double -> ProcessHandle -> ExitCode -> IO Result]) <-
        fmap unzip3 $ forM results $ \r -> case r of
            ResultCode _ -> return ([], [], \_ _ ex -> return $ ResultCode ex)
            ResultTime _ -> return ([], [], \dur _ _ -> return $ ResultTime dur)
            ResultLine _ -> return ([], [], \_ _ _ -> return $ ResultLine cmdline)
            ResultProcess _ -> return ([], [], \_ pid _ -> return $ ResultProcess $ Pid pid)
            ResultStdout    s -> do (a,b) <- buf s; return (a , [], \_ _ _ -> fmap ResultStdout b)
            ResultStderr    s -> do (a,b) <- buf s; return ([], a , \_ _ _ -> fmap ResultStderr b)
            ResultStdouterr s -> do (a,b) <- buf s; return (a , a , \_ _ _ -> fmap ResultStdouterr b)

    exceptionBuffer <- newBuffer
    po <- resolvePath ProcessOpts
        {poCommand = if optShell then ShellCommand $ unwords $ exe:args else RawCommand exe args
        ,poCwd = optCwd, poEnv = optEnv, poTimeout = optTimeout
        ,poStdin = [SrcBytes LBS.empty | optBinary && not (null optStdin)] ++ optStdin
        ,poStdout = [DestEcho | optEchoStdout] ++ map DestFile optFileStdout ++ [DestString exceptionBuffer | optWithStdout && not optAsync] ++ concat dStdout
        ,poStderr = [DestEcho | optEchoStderr] ++ map DestFile optFileStderr ++ [DestString exceptionBuffer | optWithStderr && not optAsync] ++ concat dStderr
        ,poAsync = optAsync
        }
    res <- try_ $ duration $ process po

    let failure extra = do
            cwd <- case optCwd of
                Nothing -> return ""
                Just v -> do
                    v <- canonicalizePath v `catch_` const (return v)
                    return $ "Current directory: " ++ v ++ "\n"
            fail $
                "Development.Shake." ++ funcName ++ ", system command failed\n" ++
                "Command: " ++ cmdline ++ "\n" ++
                cwd ++ extra
    case res of
        Left err -> failure $ show err
        Right (_,(_,ex)) | ex /= ExitSuccess && ResultCode ExitSuccess `notElem` results -> do
            exceptionBuffer <- readBuffer exceptionBuffer
            let captured = ["Stderr" | optWithStderr] ++ ["Stdout" | optWithStdout]
            failure $
                "Exit code: " ++ show (case ex of ExitFailure i -> i; _ -> 0) ++ "\n" ++
                if null captured then "Stderr not captured because WithStderr False was used\n"
                else if null exceptionBuffer then intercalate " and " captured ++ " " ++ (if length captured == 1 then "was" else "were") ++ " empty"
                else intercalate " and " captured ++ ":\n" ++ unlines (dropWhile null $ lines $ concat exceptionBuffer)
        Right (dur,(pid,ex)) -> mapM (\f -> f dur pid ex) resultBuild


-- | Apply all environment operations, to produce a new environment to use.
resolveEnv :: [CmdOption] -> IO (Maybe [(String, String)])
resolveEnv opts
    | null env, null addEnv, null addPath, null remEnv = return Nothing
    | otherwise = Just . unique . tweakPath . (++ addEnv) . filter (flip notElem remEnv . fst) <$>
                  if null env then getEnvironment else return (concat env)
    where
        env = [x | Env x <- opts]
        addEnv = [(x,y) | AddEnv x y <- opts]
        remEnv = [x | RemEnv x <- opts]
        addPath = [(x,y) | AddPath x y <- opts]

        newPath mid = intercalate [searchPathSeparator] $
            concat (reverse $ map fst addPath) ++ [mid | mid /= ""] ++ concatMap snd addPath
        isPath x = (if isWindows then upper else id) x == "PATH"
        tweakPath xs | not $ any (isPath . fst) xs = ("PATH", newPath "") : xs
                     | otherwise = map (\(a,b) -> (a, if isPath a then newPath b else b)) xs

        unique = reverse . nubOrdOn (if isWindows then upper . fst else fst) . reverse


-- | If the user specifies a custom $PATH, and not Shell, then try and resolve their exe ourselves.
--   Tricky, because on Windows it doesn't look in the $PATH first.
resolvePath :: ProcessOpts -> IO ProcessOpts
resolvePath po
    | Just e <- poEnv po
    , Just (_, path) <- find ((==) "PATH" . (if isWindows then upper else id) . fst) e
    , RawCommand prog args <- poCommand po
    = do
    let progExe = if prog == prog -<.> exe then prog else prog <.> exe
    -- use unsafeInterleaveIO to allow laziness to skip the queries we don't use
    pathOld <- unsafeInterleaveIO $ fromMaybe "" <$> lookupEnv "PATH"
    old <- unsafeInterleaveIO $ findExecutable prog
    new <- unsafeInterleaveIO $ findExecutableWith (splitSearchPath path) progExe
    old2 <- unsafeInterleaveIO $ findExecutableWith (splitSearchPath pathOld) progExe

    switch <- return $ case () of
        _ | path == pathOld -> False -- The state I can see hasn't changed
          | Nothing <- new -> False -- I have nothing to offer
          | Nothing <- old -> True -- I failed last time, so this must be an improvement
          | Just old <- old, Just new <- new, equalFilePath old new -> False -- no different
          | Just old <- old, Just old2 <- old2, equalFilePath old old2 -> True -- I could predict last time
          | otherwise -> False
    return $ case new of
        Just new | switch -> po{poCommand = RawCommand new args}
        _ -> po
resolvePath po = return po


-- | Given a list of directories, and a file name, return the complete path if you can find it.
--   Like findExecutable, but with a custom PATH.
findExecutableWith :: [FilePath] -> String -> IO (Maybe FilePath)
findExecutableWith path x = flip firstJustM (map (</> x) path) $ \s ->
    ifM (doesFileExist s) (return $ Just s) (return Nothing)


---------------------------------------------------------------------
-- FIXED ARGUMENT WRAPPER

-- | Collect the @stdout@ of the process.
--   If used, the @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
newtype Stdout a = Stdout {fromStdout :: a}

-- | Collect the @stderr@ of the process.
--   If used, the @stderr@ will not be echoed to the terminal, unless you include 'EchoStderr'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
newtype Stderr a = Stderr {fromStderr :: a}

-- | Collect the @stdout@ and @stderr@ of the process.
--   If used, the @stderr@ and @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout' and 'EchoStderr'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
newtype Stdouterr a = Stdouterr {fromStdouterr :: a}

-- | Collect the 'ExitCode' of the process.
--   If you do not collect the exit code, any 'ExitFailure' will cause an exception.
newtype Exit = Exit {fromExit :: ExitCode}

-- | Collect the 'ProcessHandle' of the process.
--   If you do collect the process handle, the command will run asyncronously and the call to 'cmd' \/ 'command'
--   will return as soon as the process is spawned. Any 'Stdout' \/ 'Stderr' captures will return empty strings.
newtype Process = Process {fromProcess :: ProcessHandle}

-- | Collect the time taken to execute the process. Can be used in conjunction with 'CmdLine' to
--   write helper functions that print out the time of a result.
--
-- @
-- timer :: ('CmdResult' r, MonadIO m) => (forall r . 'CmdResult' r => m r) -> m r
-- timer act = do
--     ('CmdTime' t, 'CmdLine' x, r) <- act
--     liftIO $ putStrLn $ \"Command \" ++ x ++ \" took \" ++ show t ++ \" seconds\"
--     return r
--
-- run :: IO ()
-- run = timer $ 'cmd' \"ghc --version\"
-- @
newtype CmdTime = CmdTime {fromCmdTime :: Double}

-- | Collect the command line used for the process. This command line will be approximate -
--   suitable for user diagnostics, but not for direct execution.
newtype CmdLine = CmdLine {fromCmdLine :: String}

-- | The allowable 'String'-like values that can be captured.
class CmdString a where cmdString :: (Str, Str -> a)
instance CmdString () where cmdString = (Unit, \Unit -> ())
instance CmdString String where cmdString = (Str "", \(Str x) -> x)
instance CmdString BS.ByteString where cmdString = (BS BS.empty, \(BS x) -> x)
instance CmdString LBS.ByteString where cmdString = (LBS LBS.empty, \(LBS x) -> x)


#if __GLASGOW_HASKELL__ >= 710
class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
#else
class Unit a
instance Unit b => Unit (a -> b)
instance a ~ () => Unit (m a)
#endif


-- | A class for specifying what results you want to collect from a process.
--   Values are formed of 'Stdout', 'Stderr', 'Exit' and tuples of those.
class CmdResult a where
    -- Return a list of results (with the right type but dummy data)
    -- and a function to transform a populated set of results into a value
    cmdResult :: ([Result], [Result] -> a)

instance CmdResult Exit where
    cmdResult = ([ResultCode ExitSuccess], \[ResultCode x] -> Exit x)

instance CmdResult ExitCode where
    cmdResult = ([ResultCode ExitSuccess], \[ResultCode x] -> x)

instance CmdResult Process where
    cmdResult = ([ResultProcess Pid0], \[ResultProcess (Pid x)] -> Process x)

instance CmdResult ProcessHandle where
    cmdResult = ([ResultProcess Pid0], \[ResultProcess (Pid x)] -> x)

instance CmdResult CmdLine where
    cmdResult = ([ResultLine ""], \[ResultLine x] -> CmdLine x)

instance CmdResult CmdTime where
    cmdResult = ([ResultTime 0], \[ResultTime x] -> CmdTime x)

instance CmdString a => CmdResult (Stdout a) where
    cmdResult = let (a,b) = cmdString in ([ResultStdout a], \[ResultStdout x] -> Stdout $ b x)

instance CmdString a => CmdResult (Stderr a) where
    cmdResult = let (a,b) = cmdString in ([ResultStderr a], \[ResultStderr x] -> Stderr $ b x)

instance CmdString a => CmdResult (Stdouterr a) where
    cmdResult = let (a,b) = cmdString in ([ResultStdouterr a], \[ResultStdouterr x] -> Stdouterr $ b x)

instance CmdResult () where
    cmdResult = ([], \[] -> ())

instance (CmdResult x1, CmdResult x2) => CmdResult (x1,x2) where
    cmdResult = (a1++a2, \rs -> let (r1,r2) = splitAt (length a1) rs in (b1 r1, b2 r2))
        where (a1,b1) = cmdResult
              (a2,b2) = cmdResult

cmdResultWith :: forall b c. CmdResult b => (b -> c) -> ([Result], [Result] -> c)
cmdResultWith f = second (f .) cmdResult

instance (CmdResult x1, CmdResult x2, CmdResult x3) => CmdResult (x1,x2,x3) where
    cmdResult = cmdResultWith $ \(a,(b,c)) -> (a,b,c)

instance (CmdResult x1, CmdResult x2, CmdResult x3, CmdResult x4) => CmdResult (x1,x2,x3,x4) where
    cmdResult = cmdResultWith $ \(a,(b,c,d)) -> (a,b,c,d)

instance (CmdResult x1, CmdResult x2, CmdResult x3, CmdResult x4, CmdResult x5) => CmdResult (x1,x2,x3,x4,x5) where
    cmdResult = cmdResultWith $ \(a,(b,c,d,e)) -> (a,b,c,d,e)


-- | Execute a system command. Before running 'command' make sure you 'Development.Shake.need' any files
--   that are used by the command.
--
--   This function takes a list of options (often just @[]@, see 'CmdOption' for the available
--   options), the name of the executable (either a full name, or a program on the @$PATH@) and
--   a list of arguments. The result is often @()@, but can be a tuple containg any of 'Stdout',
--   'Stderr' and 'Exit'. Some examples:
--
-- @
-- 'command_' [] \"gcc\" [\"-c\",\"myfile.c\"]                          -- compile a file, throwing an exception on failure
-- 'Exit' c <- 'command' [] \"gcc\" [\"-c\",myfile]                     -- run a command, recording the exit code
-- ('Exit' c, 'Stderr' err) <- 'command' [] \"gcc\" [\"-c\",\"myfile.c\"]   -- run a command, recording the exit code and error output
-- 'Stdout' out <- 'command' [] \"gcc\" [\"-MM\",\"myfile.c\"]            -- run a command, recording the output
-- 'command_' ['Cwd' \"generated\"] \"gcc\" [\"-c\",myfile]               -- run a command in a directory
-- @
--
--   Unless you retrieve the 'ExitCode' using 'Exit', any 'ExitFailure' will throw an error, including
--   the 'Stderr' in the exception message. If you capture the 'Stdout' or 'Stderr', that stream will not be echoed to the console,
--   unless you use the option 'EchoStdout' or 'EchoStderr'.
--
--   If you use 'command' inside a @do@ block and do not use the result, you may get a compile-time error about being
--   unable to deduce 'CmdResult'. To avoid this error, use 'command_'.
--
--   By default the @stderr@ stream will be captured for use in error messages, and also echoed. To only echo
--   pass @'WithStderr' 'False'@, which causes no streams to be captured by Shake, and certain programs (e.g. @gcc@)
--   to detect they are running in a terminal.
command :: CmdResult r => [CmdOption] -> String -> [String] -> Action r
command opts x xs = b <$> commandExplicit "command" opts a x xs
    where (a,b) = cmdResult

-- | A version of 'command' where you do not require any results, used to avoid errors about being unable
--   to deduce 'CmdResult'.
command_ :: [CmdOption] -> String -> [String] -> Action ()
command_ opts x xs = void $ commandExplicit "command_" opts [] x xs


---------------------------------------------------------------------
-- VARIABLE ARGUMENT WRAPPER

-- | A type annotation, equivalent to the first argument, but in variable argument contexts,
--   gives a clue as to what return type is expected (not actually enforced).
type a :-> t = a


-- | Execute a system command. Before running 'cmd' make sure you 'Development.Shake.need' any files
--   that are used by the command.
--
-- * @String@ arguments are treated as whitespace separated arguments.
--
-- * @[String]@ arguments are treated as literal arguments.
--
-- * 'CmdOption' arguments are used as options.
--
--   As some examples, here are some calls, and the resulting command string:
--
-- @
-- 'cmd_' \"git log --pretty=\" \"oneline\"           -- git log --pretty= oneline
-- 'cmd_' \"git log --pretty=\" [\"oneline\"]         -- git log --pretty= oneline
-- 'cmd_' \"git log\" (\"--pretty=\" ++ \"oneline\")    -- git log --pretty=oneline
-- 'cmd_' \"git log\" (\"--pretty=\" ++ \"one line\")   -- git log --pretty=one line
-- 'cmd_' \"git log\" [\"--pretty=\" ++ \"one line\"]   -- git log "--pretty=one line"
-- @
--
--   More examples, including return values, see this translation of the examples given for the 'command' function:
--
-- @
-- 'cmd_' \"gcc -c myfile.c\"                                         -- compile a file, throwing an exception on failure
-- 'Exit' c <- 'cmd' \"gcc -c\" [myfile]                              -- run a command, recording the exit code
-- ('Exit' c, 'Stderr' err) <- 'cmd' \"gcc -c myfile.c\"                -- run a command, recording the exit code and error output
-- 'Stdout' out <- 'cmd' \"gcc -MM myfile.c\"                         -- run a command, recording the output
-- 'cmd' ('Cwd' \"generated\") \"gcc -c\" [myfile] :: 'Action' ()         -- run a command in a directory
-- @
--
--   When passing file arguments we use @[myfile]@ so that if the @myfile@ variable contains spaces they are properly escaped.
--
--   If you use 'cmd' inside a @do@ block and do not use the result, you may get a compile-time error about being
--   unable to deduce 'CmdResult'. To avoid this error, use 'cmd_'.
--
--   The 'cmd' function can also be run in the 'IO' monad, but then 'Traced' is ignored and command lines are not echoed.
--   As an example:
--
-- @
-- 'cmd' ('Cwd' \"generated\") 'Shell' \"gcc -c myfile.c\" :: IO ()
-- @
cmd :: CmdArguments args => args :-> Action r
cmd = cmdArguments mempty

-- | See 'cmd'. Same as 'cmd' except with a unit result.
-- 'cmd' is to 'cmd_' as 'command' is to 'command_'.
cmd_ :: (CmdArguments args, Unit args) => args :-> Action ()
cmd_ = cmd

-- | The arguments to 'cmd' - see 'cmd' for examples and semantics.
newtype CmdArgument = CmdArgument [Either CmdOption String]
  deriving (Eq, Monoid, Show)

-- | The arguments to 'cmd' - see 'cmd' for examples and semantics.
class CmdArguments t where
    -- | Arguments to cmd
    cmdArguments :: CmdArgument -> t
instance (IsCmdArgument a, CmdArguments r) => CmdArguments (a -> r) where
    cmdArguments xs x = cmdArguments $ xs `mappend` toCmdArgument x
instance CmdResult r => CmdArguments (Action r) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in b <$> commandExplicit "cmd" opts a x xs
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"
instance CmdResult r => CmdArguments (IO r) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in b <$> commandExplicitIO "cmd" opts a x xs
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"
instance CmdArguments CmdArgument where
    cmdArguments = id

-- | Class to convert an a  to a CmdArgument
class IsCmdArgument a where
    -- | Conversion to a CmdArgument
    toCmdArgument :: a -> CmdArgument
instance IsCmdArgument String where toCmdArgument = CmdArgument . map Right . words
instance IsCmdArgument [String] where toCmdArgument = CmdArgument . map Right
instance IsCmdArgument CmdOption where toCmdArgument = CmdArgument . return . Left
instance IsCmdArgument [CmdOption] where toCmdArgument = CmdArgument . map Left
instance IsCmdArgument a => IsCmdArgument (Maybe a) where toCmdArgument = maybe mempty toCmdArgument


---------------------------------------------------------------------
-- UTILITIES

-- A better version of showCommandForUser, which doesn't escape so much on Windows
showCommandForUser2 :: FilePath -> [String] -> String
showCommandForUser2 cmd args = unwords $ map (\x -> if safe x then x else showCommandForUser x []) $ cmd : args
    where
        safe xs = not (null xs) && not (any bad xs)
        bad x = isSpace x || (x == '\\' && not isWindows) || x `elem` "\"\'"
