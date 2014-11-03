{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeOperators #-}

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
    command, command_, cmd, CmdArguments,
    Stdout(..), Stderr(..), Exit(..),
    CmdResult, CmdOption(..),
    addPath, addEnv,
    ) where

import Data.Tuple.Extra
import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Extra as C
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Foreign.C.Error
import System.Directory
import System.Environment
import System.Exit
import System.IO.Extra
import System.Process

import Development.Shake.Core
import Development.Shake.FilePath
import Development.Shake.Types
import Development.Shake.Rules.File
import General.Base

import GHC.IO.Exception (IOErrorType(..), IOException(..))


---------------------------------------------------------------------
-- ACTUAL EXECUTION

-- | Options passed to 'command' or 'cmd' to control how processes are executed.
data CmdOption
    = Cwd FilePath -- ^ Change the current directory in the spawned process. By default uses this processes current directory.
    | Env [(String,String)] -- ^ Change the environment variables in the spawned process. By default uses this processes environment.
                            --   Use 'addPath' to modify the @$PATH@ variable, or 'addEnv' to modify other variables.
    | Stdin String -- ^ Given as the @stdin@ of the spawned process. By default the @stdin@ is inherited.
    | Shell -- ^ Pass the command to the shell without escaping - any arguments will be joined with spaces. By default arguments are escaped properly.
    | BinaryPipes -- ^ Treat the @stdin@\/@stdout@\/@stderr@ messages as binary. By default streams use text encoding.
    | Traced String -- ^ Name to use with 'traced', or @\"\"@ for no tracing. By default traces using the name of the executable.
    | WithStderr Bool -- ^ Should I include the @stderr@ in the exception if the command fails? Defaults to 'True'.
    | EchoStdout Bool -- ^ Should I echo the @stdout@? Defaults to 'True' unless a 'Stdout' result is required.
    | EchoStderr Bool -- ^ Should I echo the @stderr@? Defaults to 'True' unless a 'Stderr' result is required.
      deriving (Eq,Ord,Show)


-- | Produce a 'CmdOption' of value 'Env' that is the current environment, plus a
--   prefix and suffix to the @$PATH@ environment variable. For example:
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
    let (path,other) = partition ((== "PATH") . (if isWindows then map toUpper else id) . fst) args
    return $ Env $
        [("PATH",intercalate [searchPathSeparator] $ pre ++ post) | null path] ++
        [(a,intercalate [searchPathSeparator] $ pre ++ [b | b /= ""] ++ post) | (a,b) <- path] ++
        other

-- | Produce a 'CmdOption' of value 'Env' that is the current environment, plus the argument
--   environment variables. For example:
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
    return $ Env $ extra ++ filter (\(a,b) -> a `notElem` map fst extra) args


data Result
    = ResultStdout String
    | ResultStderr String
    | ResultCode ExitCode
      deriving Eq


commandExplicit :: String -> [CmdOption] -> [Result] -> String -> [String] -> Action [Result]
commandExplicit funcName copts results exe args = do
        opts <- getShakeOptions
        verb <- getVerbosity

        let skipper act = if null results && not (shakeRunCommands opts) then return [] else act

        let verboser act = do
                let cwd = listToMaybe $ reverse [x | Cwd x <- copts]
                putLoud $ maybe "" (\x -> "cd " ++ x ++ "; ") cwd ++ saneCommandForUser exe args
                (if verb >= Loud then quietly else id) act

        let tracer = case reverse [x | Traced x <- copts] of
                "":_ -> liftIO
                msg:_ -> traced msg
                [] -> traced (takeFileName exe)

        let tracker act = case shakeLint opts of
                Just LintTracker -> do
                    dir <- liftIO $ getTemporaryDirectory
                    (file, handle) <- liftIO $ openTempFile dir "shake.lint"
                    liftIO $ hClose handle
                    dir <- return $ file <.> "dir"
                    liftIO $ createDirectory dir
                    let cleanup = removeDirectoryRecursive dir >> removeFile file
                    flip actionFinally cleanup $ do
                        res <- act "tracker" $ "/if":dir:"/c":exe:args
                        (read,write) <- liftIO $ trackerFiles dir
                        trackRead read
                        trackWrite write
                        return res
                _ -> act exe args

        skipper $ tracker $ \exe args -> verboser $ tracer $ commandExplicitIO funcName copts results exe args


-- | Given a directory (as passed to tracker /if) report on which files were used for reading/writing
trackerFiles :: FilePath -> IO ([FilePath], [FilePath])
trackerFiles dir = do
    curdir <- getCurrentDirectory
    let pre = map toUpper curdir ++ "\\"
    files <- getDirectoryContents dir
    let f typ = do
            files <- forM [x | x <- files, takeExtension x == ".tlog", takeExtension (dropExtension $ dropExtension x) == '.':typ] $ \file -> do
                xs <- readFileEncoding utf16 $ dir </> file
                return $ filter (not . isPrefixOf "." . takeFileName) . mapMaybe (stripPrefix pre) $ lines xs
            fmap nub $ mapMaybeM correctCase $ nub $ concat files
    liftM2 (,) (f "read") (f "write")


correctCase :: FilePath -> IO (Maybe FilePath)
correctCase x = f "" x
    where
        f pre "" = return $ Just pre
        f pre x = do
            let (a,b) = (takeDirectory1 x, dropDirectory1 x)
            dir <- getDirectoryContents pre
            case find ((==) a . map toUpper) dir of
                Nothing -> return Nothing -- if it can't be found it probably doesn't exist, so assume a file that wasn't really read
                Just v -> f (pre +/+ v) b

        a +/+ b = if null a then b else a ++ "/" ++ b


commandExplicitIO :: String -> [CmdOption] -> [Result] -> String -> [String] -> IO [Result]
commandExplicitIO funcName opts results exe args =
-- BEGIN COPIED
-- Originally from readProcessWithExitCode with as few changes as possible
    mask $ \restore -> do
      ans <- try_ $ createProcess cp
      (inh, outh, errh, pid) <- case ans of
          Right a -> return a
          Left err -> failure $ show err

      let close = maybe (return ()) hClose
      flip onException
        (do close inh; close outh; close errh
            terminateProcess pid; waitForProcess pid) $ restore $ do

        -- set pipes to binary if appropriate
        when (BinaryPipes `elem` opts) $ do
            let bin = maybe (return ()) (`hSetBinaryMode` True)
            bin inh; bin outh; bin errh

        -- fork off a thread to start consuming stdout
        (out,waitOut,waitOutEcho) <- case outh of
            Nothing -> return ("", return (), return ())
            Just outh -> do
                out <- hGetContents outh
                waitOut <- forkWait $ C.evaluate $ rnf out
                waitOutEcho <- if stdoutEcho
                                 then forkWait (hPutStr stdout out)
                                 else return (return ())
                return (out,waitOut,waitOutEcho)

        -- fork off a thread to start consuming stderr
        (err,waitErr,waitErrEcho) <- case errh of
            Nothing -> return ("", return (), return ())
            Just errh -> do
                err <- hGetContents errh
                waitErr <- forkWait $ C.evaluate $ rnf err
                waitErrEcho <- if stderrEcho
                                 then forkWait (hPutStr stderr err)
                                 else return (return ())
                return (err,waitErr,waitErrEcho)

        -- now write and flush any input
        let writeInput = do
              case inh of
                  Nothing -> return ()
                  Just inh -> do
                      hPutStr inh input
                      hFlush inh
                      hClose inh

        C.catch writeInput $ \e -> case e of
          IOError { ioe_type = ResourceVanished
                  , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
          _ -> throwIO e

        -- wait on the output
        waitOut
        waitErr

        waitOutEcho
        waitErrEcho

        close outh
        close errh

        -- wait on the process
        ex <- waitForProcess pid
-- END COPIED

        when (ResultCode ExitSuccess `notElem` results && ex /= ExitSuccess) $ do
            failure $
                "Exit code: " ++ show (case ex of ExitFailure i -> i; _ -> 0) ++ "\n" ++
                (if not stderrThrow then "Stderr not captured because ErrorsWithoutStderr was used"
                else if null err then "Stderr was empty"
                else "Stderr:\n" ++ unlines (dropWhile null $ lines err))

        return $ flip map results $ \x -> case x of
            ResultStdout _ -> ResultStdout out
            ResultStderr _ -> ResultStderr err
            ResultCode   _ -> ResultCode ex
    where
        failure extra = do
            cwd <- case cwd cp of
                Nothing -> return ""
                Just v -> do
                    v <- canonicalizePath v `catch_` const (return v)
                    return $ "Current directory: " ++ v ++ "\n"
            fail $
                "Development.Shake." ++ funcName ++ ", system command failed\n" ++
                "Command: " ++ saneCommandForUser exe args ++ "\n" ++
                cwd ++ extra

        input = last $ "" : [x | Stdin x <- opts]

        -- what should I do with these handles
        binary = BinaryPipes `elem` opts
        stdoutEcho = last $ (ResultStdout "" `notElem` results) : [b | EchoStdout b <- opts]
        stdoutCapture = ResultStdout "" `elem` results
        stderrEcho = last $ (ResultStderr "" `notElem` results) : [b | EchoStderr b <- opts]
        stderrThrow = last $ True : [b | WithStderr b <- opts]
        stderrCapture = ResultStderr "" `elem` results || (stderrThrow && ResultCode ExitSuccess `notElem` results)

        cp0 = (if Shell `elem` opts then shell $ unwords $ exe:args else proc exe args)
            {std_out = if binary || stdoutCapture || not stdoutEcho then CreatePipe else Inherit
            ,std_err = if binary || stderrCapture || not stderrEcho then CreatePipe else Inherit
            ,std_in  = if null input then Inherit else CreatePipe
            }
        cp = foldl applyOpt cp0{std_out = CreatePipe, std_err = CreatePipe} opts
        applyOpt :: CreateProcess -> CmdOption -> CreateProcess
        applyOpt o (Cwd x) = o{cwd = if x == "" then Nothing else Just x}
        applyOpt o (Env x) = o{env = Just x}
        applyOpt o _ = o


-- Copied from System.Process
forkWait :: IO a -> IO (IO a)
forkWait a = do
    res <- newEmptyMVar
    _ <- mask $ \restore -> forkIO $ try_ (restore a) >>= putMVar res
    return (takeMVar res >>= either throwIO return)


-- Like System.Process, but tweaked to show less escaping,
-- Relies on relatively detailed internals of showCommandForUser.
saneCommandForUser :: FilePath -> [String] -> String
saneCommandForUser cmd args = unwords $ map f $ cmd:args
    where
        f x = if take (length y - 2) (drop 1 y) == x then x else y
            where y = showCommandForUser x []


---------------------------------------------------------------------
-- FIXED ARGUMENT WRAPPER

-- | Collect the @stdout@ of the process.
--   If you are collecting the @stdout@, it will not be echoed to the terminal, unless you include 'EchoStdout'.
newtype Stdout = Stdout {fromStdout :: String}

-- | Collect the @stderr@ of the process.
--   If you are collecting the @stderr@, it will not be echoed to the terminal, unless you include 'EchoStderr'.
newtype Stderr = Stderr {fromStderr :: String}

-- | Collect the 'ExitCode' of the process.
--   If you do not collect the exit code, any 'ExitFailure' will cause an exception.
newtype Exit = Exit {fromExit :: ExitCode}

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

instance CmdResult Stdout where
    cmdResult = ([ResultStdout ""], \[ResultStdout x] -> Stdout x)

instance CmdResult Stderr where
    cmdResult = ([ResultStderr ""], \[ResultStderr x] -> Stderr x)

instance CmdResult () where
    cmdResult = ([], \[] -> ())

instance (CmdResult x1, CmdResult x2) => CmdResult (x1,x2) where
    cmdResult = (a1++a2, \rs -> let (r1,r2) = splitAt (length a1) rs in (b1 r1, b2 r2))
        where (a1,b1) = cmdResult
              (a2,b2) = cmdResult

cmdResultWith f = second (f .) cmdResult

instance (CmdResult x1, CmdResult x2, CmdResult x3) => CmdResult (x1,x2,x3) where
    cmdResult = cmdResultWith $ \(a,(b,c)) -> (a,b,c)


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
command :: CmdResult r => [CmdOption] -> String -> [String] -> Action r
command opts x xs = fmap b $ commandExplicit "command" opts a x xs
    where (a,b) = cmdResult

-- | A version of 'command' where you do not require any results, used to avoid errors about being unable
--   to deduce 'CmdResult'.
command_ :: [CmdOption] -> String -> [String] -> Action ()
command_ opts x xs = void $ commandExplicit "command_" opts [] x xs


---------------------------------------------------------------------
-- VARIABLE ARGUMENT WRAPPER

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
--   To take the examples from 'command':
--
-- @
-- () <- 'cmd' \"gcc -c myfile.c\"                                  -- compile a file, throwing an exception on failure
-- 'Exit' c <- 'cmd' \"gcc -c\" [myfile]                              -- run a command, recording the exit code
-- ('Exit' c, 'Stderr' err) <- 'cmd' \"gcc -c myfile.c\"                -- run a command, recording the exit code and error output
-- 'Stdout' out <- 'cmd' \"gcc -MM myfile.c\"                         -- run a command, recording the output
-- 'cmd' ('Cwd' \"generated\") \"gcc -c\" [myfile] :: 'Action' ()         -- run a command in a directory
-- @
--
--   When passing file arguments we use @[myfile]@ so that if the @myfile@ variable contains spaces they are properly escaped.
--
--   If you use 'cmd' inside a @do@ block and do not use the result, you may get a compile-time error about being
--   unable to deduce 'CmdResult'. To avoid this error, bind the result to @()@, or include a type signature.
--
--   The 'cmd' command can also be run in the 'IO' monad, but then 'Traced' is ignored and command lines are not echoed.
cmd :: CmdArguments args => args :-> Action r
cmd = cmdArguments []

class CmdArguments t where cmdArguments :: [Either CmdOption String] -> t
instance (Arg a, CmdArguments r) => CmdArguments (a -> r) where
    cmdArguments xs x = cmdArguments $ xs ++ arg x
instance CmdResult r => CmdArguments (Action r) where
    cmdArguments x = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in fmap b $ commandExplicit "cmd" opts a x xs
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"
instance CmdResult r => CmdArguments (IO r) where
    cmdArguments x = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in fmap b $ commandExplicitIO "cmd" opts a x xs
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

class Arg a where arg :: a -> [Either CmdOption String]
instance Arg String where arg = map Right . words
instance Arg [String] where arg = map Right
instance Arg (Maybe String) where arg = map Right . maybe [] words
instance Arg CmdOption where arg = return . Left
instance Arg [CmdOption] where arg = map Left
instance Arg (Maybe CmdOption) where arg = map Left . maybeToList
