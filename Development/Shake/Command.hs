{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeOperators #-}

-- | /Deprecated:/ This module should no longer be imported as all the functions are available directly
--   from "Development.Shake". In future versions this module will be removed.
module Development.Shake.Command(
    command, command_, cmd,
    Stdout(..), Stderr(..), Exit(..),
    CmdResult, CmdOption(..),
    ) where

import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Foreign.C.Error
import System.Exit
import System.IO
import System.Process

import Development.Shake.Core
import Development.Shake.FilePath
import Development.Shake.Types

import GHC.IO.Exception (IOErrorType(..), IOException(..))


---------------------------------------------------------------------
-- ACTUAL EXECUTION

-- | Options passed to 'command' or 'cmd' to control how processes are executed.
data CmdOption
    = Cwd FilePath -- ^ Change the current directory in the spawned process. By default uses this processes current directory.
    | Env [(String,String)] -- ^ Change the environment variables in the spawned process. By default uses this processes environment.
    | Stdin String -- ^ Given as the @stdin@ of the spawned process. By default no @stdin@ is given.
    | Shell -- ^ Pass the command to the shell without escaping - any arguments will be joined with spaces. By default arguments are escaped properly.
    | BinaryPipes -- ^ Treat the @stdin@\/@stdout@\/@stderr@ messages as binary. By default streams use text encoding.
    | Traced String -- ^ Name to use with 'traced', or @\"\"@ for no tracing. By default traces using the name of the executable.
    | WithStderr Bool -- ^ Should I include the @stderr@ in the exception if the command fails? Defaults to 'True'.
    | EchoStdout Bool -- ^ Should I echo the @stdout@? Defaults to 'True' unless a 'Stdout' result is required.
    | EchoStderr Bool -- ^ Should I echo the @stderr@? Defaults to 'True' unless a 'Stderr' result is required.
      deriving (Eq,Ord,Show)

data Result
    = ResultStdout String
    | ResultStderr String
    | ResultCode ExitCode
      deriving Eq


commandExplicit :: String -> [CmdOption] -> [Result] -> String -> [String] -> Action [Result]
commandExplicit funcName opts results exe args = verboser $ tracer $
-- BEGIN COPIED
-- Originally from readProcessWithExitCode with as few changes as possible
    mask $ \restore -> do
      ans <- try $ createProcess cp
      (inh, outh, errh, pid) <- case ans of
          Right a -> return a
          Left err -> do
              let msg = "Development.Shake." ++ funcName ++ ", system command failed\n" ++
                        "Command: " ++ saneCommandForUser exe args ++ "\n" ++
                        show (err :: SomeException)
              error msg

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
            let msg = "Development.Shake." ++ funcName ++ ", system command failed\n" ++
                      "Command: " ++ saneCommandForUser exe args ++ "\n" ++
                      "Exit code: " ++ show (case ex of ExitFailure i -> i; _ -> 0) ++ "\n" ++
                      (if not stderrThrow then "Stderr not captured because ErrorsWithoutStderr was used"
                       else if null err then "Stderr was empty"
                       else "Stderr:\n" ++ unlines (dropWhile null $ lines err))
            error msg

        return $ flip map results $ \x -> case x of
            ResultStdout _ -> ResultStdout out
            ResultStderr _ -> ResultStderr err
            ResultCode   _ -> ResultCode ex
    where
        input = last $ "" : [x | Stdin x <- opts]
        verboser act = do
            v <- getVerbosity
            putLoud $ saneCommandForUser exe args
            (if v >= Loud then quietly else id) act
        tracer = case reverse [x | Traced x <- opts] of
            "":_ -> liftIO
            msg:_ -> traced msg
            [] -> traced (takeFileName exe)

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
    _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
    return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)


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
    cmdResult = ([ResultCode $ ExitSuccess], \[ResultCode x] -> Exit x)

instance CmdResult ExitCode where
    cmdResult = ([ResultCode $ ExitSuccess], \[ResultCode x] -> x)

instance CmdResult Stdout where
    cmdResult = ([ResultStdout ""], \[ResultStdout x] -> Stdout x)

instance CmdResult Stderr where
    cmdResult = ([ResultStderr ""], \[ResultStderr x] -> Stderr x)

instance CmdResult () where
    cmdResult = ([], \[] -> ())

instance (CmdResult x1, CmdResult x2) => CmdResult (x1,x2) where
    cmdResult = (a1++a2, \rs -> let (r1,r2) = splitAt (length a2) rs in (b1 r1, b2 r2))
        where (a1,b1) = cmdResult
              (a2,b2) = cmdResult

cmdResultWith f = second (f .) cmdResult

instance (CmdResult x1, CmdResult x2, CmdResult x3) => CmdResult (x1,x2,x3) where
    cmdResult = cmdResultWith $ \(a,(b,c)) -> (a,b,c)


-- | Execute a system command. Before running 'command' make sure you 'Development.Shake.need' any files
--   that are required by the command.
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
command_ opts x xs = commandExplicit "command_" opts [] x xs >> return ()


---------------------------------------------------------------------
-- VARIABLE ARGUMENT WRAPPER

type a :-> t = a


-- | A variable arity version of 'command'.
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
cmd :: CmdArguments args => args :-> Action r
cmd = cmdArguments []

class CmdArguments t where cmdArguments :: [Either CmdOption String] -> t
instance (Arg a, CmdArguments r) => CmdArguments (a -> r) where
    cmdArguments xs x = cmdArguments $ xs ++ arg x
instance CmdResult r => CmdArguments (Action r) where
    cmdArguments x = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in fmap b $ commandExplicit "cmd" opts a x xs
        _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

class Arg a where arg :: a -> [Either CmdOption String]
instance Arg String where arg = map Right . words
instance Arg [String] where arg = map Right
instance Arg CmdOption where arg = return . Left
instance Arg [CmdOption] where arg = map Left
