{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | A wrapping of createProcess to provide a more flexible interface.
module General.Process(
    Buffer, newBuffer, readBuffer,
    process, ProcessOpts(..), Source(..), Destination(..)
    ) where

import Control.Concurrent.Extra
import Control.DeepSeq
import Control.Exception.Extra as C
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Foreign.C.Error
import System.Exit
import System.IO.Extra
import System.Info.Extra
import System.Process
import System.Time.Extra
import Data.Unique
import Data.IORef.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import General.Extra
import Development.Shake.Internal.Errors

import GHC.IO.Exception (IOErrorType(..), IOException(..))

---------------------------------------------------------------------
-- BUFFER ABSTRACTION

data Buffer a = Buffer Unique (IORef [a])
instance Eq (Buffer a) where Buffer x _ == Buffer y _ = x == y
instance Ord (Buffer a) where compare (Buffer x _) (Buffer y _) = compare x y

newBuffer :: IO (Buffer a)
newBuffer = liftM2 Buffer newUnique (newIORef [])

addBuffer :: Buffer a -> a -> IO ()
addBuffer (Buffer _ ref) x = atomicModifyIORef_ ref (x:)

readBuffer :: Buffer a -> IO [a]
readBuffer (Buffer _ ref) = reverse <$> readIORef ref


---------------------------------------------------------------------
-- OPTIONS

data Source
    = SrcFile FilePath
    | SrcString String
    | SrcBytes LBS.ByteString

data Destination
    = DestEcho
    | DestFile FilePath
    | DestString (Buffer String)
    | DestBytes (Buffer BS.ByteString)
      deriving (Eq,Ord)

isDestString DestString{} = True; isDestString _ = False
isDestBytes  DestBytes{}  = True; isDestBytes  _ = False

data ProcessOpts = ProcessOpts
    {poCommand :: CmdSpec
    ,poCwd :: Maybe FilePath
    ,poEnv :: Maybe [(String, String)]
    ,poTimeout :: Maybe Double
    ,poStdin :: [Source]
    ,poStdout :: [Destination]
    ,poStderr :: [Destination]
    ,poAsync :: Bool
    ,poCloseFds :: Bool
    }


---------------------------------------------------------------------
-- IMPLEMENTATION

-- | If two buffers can be replaced by one and a copy, do that (only if they start empty)
optimiseBuffers :: ProcessOpts -> IO (ProcessOpts, IO ())
optimiseBuffers po@ProcessOpts{..} = return (po{poStdout = nubOrd poStdout, poStderr = nubOrd poStderr}, pure ())

stdStream :: (FilePath -> Handle) -> [Destination] -> [Destination] -> StdStream
stdStream _ [DestEcho] _ = Inherit
stdStream file [DestFile x] other | other == [DestFile x] || DestFile x `notElem` other = UseHandle $ file x
stdStream _ _ _ = CreatePipe


stdIn :: (FilePath -> Handle) -> [Source] -> (StdStream, Handle -> IO ())
stdIn _ [] = (Inherit, const $ pure ())
stdIn file [SrcFile x] = (UseHandle $ file x, const $ pure ())
stdIn file src = (,) CreatePipe $ \h -> ignoreSigPipe $ do
    forM_ src $ \case
        SrcString x -> hPutStr h x
        SrcBytes x -> LBS.hPutStr h x
        SrcFile x -> LBS.hPutStr h =<< LBS.hGetContents (file x)
    hClose h


ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handleIO $ \e -> case e of
    IOError {ioe_type=ResourceVanished, ioe_errno=Just ioe} | Errno ioe == ePIPE -> return ()
    _ -> throwIO e


withExceptions :: IO () -> IO a -> IO a
withExceptions stop go = do
    bar <- newBarrier
    v <- mask $ \unmask -> do
        forkFinally (unmask go) $ signalBarrier bar
        unmask (waitBarrier bar) `onException` do
            forkIO stop
            waitBarrier bar
    either throwIO pure v


withTimeout :: Maybe Double -> IO () -> IO a -> IO a
withTimeout Nothing _ go = go
withTimeout (Just s) stop go = bracket (forkIO $ sleep s >> stop) killThread $ const go


cmdSpec :: CmdSpec -> CreateProcess
cmdSpec (ShellCommand x) = shell x
cmdSpec (RawCommand x xs) = proc x xs



forkWait :: IO a -> IO (IO a)
forkWait a = do
    res <- newEmptyMVar
    _ <- mask $ \restore -> forkIO $ try_ (restore a) >>= putMVar res
    return $ takeMVar res >>= either throwIO pure


abort :: ProcessHandle -> IO ()
abort pid = do
    interruptProcessGroupOf pid
    sleep 3 -- give the process a few seconds grace period to die nicely
    -- seems to happen with some GHC 7.2 compiled binaries with FFI etc
    terminateProcess pid

withFiles :: IOMode -> [FilePath] -> ((FilePath -> Handle) -> IO a) -> IO a
withFiles mode files act = withs (map (`withFile` mode) files) $ \handles ->
    act $ \x -> fromJust $ lookup x $ zipExact files handles


-- General approach taken from readProcessWithExitCode
process :: ProcessOpts -> IO (ProcessHandle, ExitCode)
process po = do
    (ProcessOpts{..}, flushBuffers) <- optimiseBuffers po
    let outFiles = nubOrd [x | DestFile x <- poStdout ++ poStderr]
    let inFiles = nubOrd [x | SrcFile x <- poStdin]
    withFiles WriteMode outFiles $ \outHandle -> withFiles ReadMode inFiles $ \inHandle -> do
        let cp = (cmdSpec poCommand){cwd = poCwd, env = poEnv, create_group = True, close_fds = poCloseFds
                 ,std_in = fst $ stdIn inHandle poStdin
                 ,std_out = stdStream outHandle poStdout poStderr, std_err = stdStream outHandle poStderr poStdout}
        withCreateProcessCompat cp $ \inh outh errh pid ->
            withExceptions (abort pid) $ withTimeout poTimeout (abort pid) $ do

                let streams = [(outh, stdout, poStdout) | Just outh <- [outh], CreatePipe <- [std_out cp]] ++
                              [(errh, stderr, poStderr) | Just errh <- [errh], CreatePipe <- [std_err cp]]
                wait <- forM streams $ \(h, hh, dest) -> do
                    -- no point tying the streams together if one is being streamed directly
                    let isTied = not (poStdout `disjoint` poStderr) && length streams == 2
                    let isBinary = any isDestBytes dest || not (any isDestString dest)
                    when isTied $ hSetBuffering h LineBuffering
                    when (DestEcho `elem` dest) $ do
                        buf <- hGetBuffering hh
                        case buf of
                            BlockBuffering{} -> return ()
                            _ -> hSetBuffering h buf

                    if isBinary then do
                        hSetBinaryMode h True
                        dest<- pure $ flip map dest $ \case
                            DestEcho -> BS.hPut hh
                            DestFile x -> BS.hPut (outHandle x)
                            DestString x -> addBuffer x . (if isWindows then replace "\r\n" "\n" else id) . BS.unpack
                            DestBytes x -> addBuffer x
                        forkWait $ whileM $ do
                            src <- BS.hGetSome h 4096
                            mapM_ ($ src) dest
                            notM $ hIsEOF h
                     else if isTied then do
                        dest<- pure $ flip map dest $ \case
                            DestEcho -> hPutStrLn hh
                            DestFile x -> hPutStrLn (outHandle x)
                            DestString x -> addBuffer x . (++ "\n")
                            DestBytes{} -> throwImpure $ errorInternal "Not reachable due to isBinary condition"
                        forkWait $ whileM $
                            ifM (hIsEOF h) (pure False) $ do
                                src <- hGetLine h
                                mapM_ ($ src) dest
                                pure True
                     else do
                        src <- hGetContents h
                        wait1 <- forkWait $ C.evaluate $ rnf src
                        waits <- forM dest $ \case
                            DestEcho -> forkWait $ hPutStr hh src
                            DestFile x -> forkWait $ hPutStr (outHandle x) src
                            DestString x -> do addBuffer x src; return $ return ()
                            DestBytes{} -> throwImpure $ errorInternal "Not reachable due to isBinary condition"
                        pure $ sequence_ $ wait1 : waits

                whenJust inh $ snd $ stdIn inHandle poStdin
                if poAsync then
                    pure (pid, ExitSuccess)
                 else do
                    sequence_ wait
                    flushBuffers
                    res <- waitForProcess pid
                    whenJust outh hClose
                    whenJust errh hClose
                    pure (pid, res)


---------------------------------------------------------------------
-- COMPATIBILITY

-- available in process-1.4.3.0, GHC ??? (Nov 2015)
-- logic copied directly (apart from Ctrl-C handling magic using internal pieces)
withCreateProcessCompat :: CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcessCompat cp act = bracketOnError (createProcess cp) cleanup
    (\(m_in, m_out, m_err, ph) -> act m_in m_out m_err ph)
    where
        cleanup (inh, outh, errh, pid) = do
            terminateProcess pid
            whenJust inh $ ignoreSigPipe . hClose
            whenJust outh hClose
            whenJust errh hClose
            forkIO $ void $ waitForProcess pid
