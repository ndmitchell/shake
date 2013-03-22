{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeOperators #-}

-- | /Future plans: I intend to merge this module into "Development.Shake" itself./
--
--   This module provides more powerful and flexible versions of 'Development.Shake.system''.
--   The best documentation is found at 'command'.
module Development.Shake.Command(
    cmd, command, command_,
    Stdout(..), Stderr(..), Exit(..),
    CmdOption(..),
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

import GHC.IO.Exception (IOErrorType(..), IOException(..))



---------------------------------------------------------------------
-- ACTUAL EXECUTION

-- | Options passed to 'command' to control how processes are executed.
data CmdOption
    = Cwd FilePath -- ^ Change the current directory in the spawned process.
    | Env [(String,String)] -- ^ Change the environment variables in the spawned process.
    | Stdin String -- ^ Given as the @stdin@ of the spawned process.
    | ErrorsWithoutStderr -- ^ By default, the @stderr@ is captured and included if the command fails. This option disables that behavior.
    | EchoStdout -- ^ If a 'Stdout' result is requested, the @stdout@ is not normally echoed, this option causes it to echo again.
    | EchoStderr -- ^ If a 'Stderr' result is requested, the @stderr@ is not normally echoed, this option causes it to echo again.
    | Shell -- ^ Pass the command to the shell without escaping - any arguments will be joined with spaces.
    | BinaryPipes -- ^ Treat the @stdin@/@stdout@/@stderr@ messages as binary.
    | Traced String -- ^ Name to use with 'traced', defaulting to the name of the executable. Use @\"\"@ for no tracing.
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
        (out,waitOut) <- case outh of
            Nothing -> return ("", return ())
            Just outh -> do
                out <- hGetContents outh
                waitOut <- forkWait $ C.evaluate $ rnf out
                when stdoutEcho $ forkIO (hPutStr stdout out) >> return ()
                return (out,waitOut)

        -- fork off a thread to start consuming stderr
        (err,waitErr) <- case errh of
            Nothing -> return ("", return ())
            Just errh -> do
                err <- hGetContents errh
                waitErr <- forkWait $ C.evaluate $ rnf err
                when stderrEcho $ forkIO (hPutStr stderr err) >> return ()
                return (err,waitErr)

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

        close outh
        close errh

        -- wait on the process
        ex <- waitForProcess pid
-- END COPIED

        when (ResultCode ExitSuccess `notElem` results && ex /= ExitSuccess) $ do
            let msg = "Development.Shake." ++ funcName ++ ", system command failed\n" ++
                      "Command: " ++ saneCommandForUser exe args ++ "\n" ++
                      "Exit code: " ++ show (case ex of ExitFailure i -> i; _ -> 0) ++ "\n" ++
                      (if ErrorsWithoutStderr `elem` opts then "Stderr not captured because ErrorsWithoutStderr was used"
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
            (if verbosity >= Loud then quietly else id) act
        tracer = case reverse [x | Traced x <- opts] of
            "":_ -> liftIO
            msg:_ -> traced msg
            [] -> traced (takeFileName exe)

        -- what should I do with these handles
        binary = BinaryPipes `elem` opts
        stdoutEcho = ResultStdout "" `notElem` results || EchoStdout `elem` opts
        stdoutCapture = ResultStdout "" `elem` results
        stderrEcho = ResultStderr "" `notElem` results || EchoStderr `elem` opts
        stderrCapture = ResultStderr "" `elem` results || ErrorsWithoutStderr `notElem` opts

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
--   If you are collecting the @stdout@, it will not be echoed to the terminal, unless you include 'StdoutEcho'.
newtype Stdout = Stdout String

-- | Collect the @stderr@ of the process.
--   If you are collecting the @stderr@, it will not be echoed to the terminal, unless you include 'StderrEcho'.
newtype Stderr = Stderr String

-- | Collect the 'ExitCode' of the process.
--   If you do not collect the exit code, any 'ExitFailure' will cause an exception.
newtype Exit = Exit ExitCode

-- | A class for specifying what results you want to collect from a process.
--   Values are formed of 'Stdout', 'Stderr', 'Exit' and tuples of those.
class CmdResult a where
    -- Return a list of results (with the right type but dummy data)
    -- and a function to transform a populated set of results into a value
    cmdResult :: ([Result], [Result] -> a)

instance CmdResult Exit where
    cmdResult = ([ResultCode $ ExitSuccess], \[ResultCode x] -> Exit x)

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


command :: CmdResult r => [CmdOption] -> String -> [String] -> Action r
command opts x xs = fmap b $ commandExplicit "command" opts a x xs
    where (a,b) = cmdResult

-- | Like 'command', but where you do not require any result.
command_ :: [CmdOption] -> String -> [String] -> Action ()
command_ opts x xs = commandExplicit "command_" opts [] x xs >> return ()


---------------------------------------------------------------------
-- VARIABLE ARGUMENT WRAPPER

type a :-> t = a


-- | A variable arity version of 'command'.
cmd :: CmdArguments args => String -> args :-> Action r
cmd x = cmdArguments $ map Right $ words x

class CmdArguments t where cmdArguments :: [Either CmdOption String] -> t
instance (Arg a, CmdArguments r) => CmdArguments (a -> r) where
    cmdArguments xs x = cmdArguments $ xs ++ arg x
instance CmdResult r => CmdArguments (Action r) where
    cmdArguments x = case partitionEithers x of
        (opts, x:xs) -> let (a,b) = cmdResult in fmap b $ commandExplicit "cmd" opts a x xs
        _ -> error "Internal error, no executable or arguments given to Development.Shake.cmd"

class Arg a where arg :: a -> [Either CmdOption String]
instance Arg String where arg = map Right . words
instance Arg [String] where arg = map Right
instance Arg CmdOption where arg = return . Left
instance Arg [CmdOption] where arg = map Left
