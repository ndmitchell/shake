{-# LANGUAGE RecordWildCards #-}

-- | A wrapping of createProcess to provide a more flexible interface.
module General.Process(
    Buffer, newBuffer, readBuffer,
    process, ProcessOpts(..), Destination(..)
    ) where

import Control.Applicative
import Control.Concurrent
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
import Data.IORef
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import General.Extra

import GHC.IO.Exception (IOErrorType(..), IOException(..))

---------------------------------------------------------------------
-- BUFFER ABSTRACTION

data Buffer a = Buffer Unique (IORef [a])
instance Eq (Buffer a) where Buffer x _ == Buffer y _ = x == y
instance Ord (Buffer a) where compare (Buffer x _) (Buffer y _) = compare x y

newBuffer :: IO (Buffer a)
newBuffer = liftM2 Buffer newUnique (newIORef [])

addBuffer :: Buffer a -> a -> IO ()
addBuffer (Buffer _ ref) x = atomicModifyIORef ref $ \xs -> (x:xs, ())

readBuffer :: Buffer a -> IO [a]
readBuffer (Buffer _ ref) = reverse <$> readIORef ref


---------------------------------------------------------------------
-- OPTIONS

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
    ,poStdin :: Either String LBS.ByteString
    ,poStdout :: [Destination]
    ,poStderr :: [Destination]
    }


---------------------------------------------------------------------
-- IMPLEMENTATION

-- | If two buffers can be replaced by one and a copy, do that (only if they start empty)
optimiseBuffers :: ProcessOpts -> IO (ProcessOpts, IO ())
optimiseBuffers po@ProcessOpts{..} = return (po{poStdout = nubOrd poStdout, poStderr = nubOrd poStderr}, return ())

stdStream :: (FilePath -> Handle) -> [Destination] -> StdStream
stdStream file [DestEcho] = Inherit
stdStream file [DestFile x] = UseHandle $ file x
stdStream file _ = CreatePipe


stdIn :: Either String LBS.ByteString -> (StdStream, Handle -> IO ())
stdIn inp | either null LBS.null inp = (Inherit, const $ return ())
          | otherwise = (,) CreatePipe $ \h ->
    void $ tryBool isPipeGone $ do
        either (hPutStr h) (LBS.hPutStr h) inp
        hFlush h
        hClose h
    where
        isPipeGone IOError{ioe_type=ResourceVanished, ioe_errno=Just ioe} = Errno ioe == ePIPE
        isPipeGone _ = False


withTimeout :: Maybe Double -> IO () -> IO a -> IO a
withTimeout Nothing stop go = go
withTimeout (Just s) stop go = bracket (forkIO $ sleep s >> stop) killThread $ const go


cmdSpec :: CmdSpec -> CreateProcess
cmdSpec (ShellCommand x) = shell x
cmdSpec (RawCommand x xs) = proc x xs



forkWait :: IO a -> IO (IO a)
forkWait a = do
    res <- newEmptyMVar
    _ <- mask $ \restore -> forkIO $ try_ (restore a) >>= putMVar res
    return $ takeMVar res >>= either throwIO return


withCreateProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withCreateProcess cp act = mask $ \restore -> do
    ans@(inh, outh, errh, pid) <- createProcess cp
    onException (restore $ act ans) $ do
        mapM_ (`whenJust` hClose) [inh, outh, errh]
        terminateProcess pid
        waitForProcess pid


-- General approach taken from readProcessWithExitCode
process :: ProcessOpts -> IO ExitCode
process po = do
    (ProcessOpts{..}, flushBuffers) <- optimiseBuffers po
    let files = nubOrd [x | DestFile x <- poStdout ++ poStderr]
    withs (map (`withFile` WriteMode) files) $ \handles -> do
        let fileHandle x = fromJust $ lookup x $ zip files handles
        let cp = (cmdSpec poCommand){cwd = poCwd, env = poEnv, create_group = isJust poTimeout
                 ,std_in = fst $ stdIn poStdin, std_out = stdStream fileHandle poStdout, std_err = stdStream fileHandle poStderr}
        withCreateProcess cp $ \(inh, outh, errh, pid) -> do
            withTimeout poTimeout (interruptProcessGroupOf pid) $ do

                let streams = [(outh, stdout, poStdout) | Just outh <- [outh]] ++ [(errh, stderr, poStderr) | Just errh <- [errh]]
                wait <- forM streams $ \(h, hh, dest) -> do
                    let isTied = not $ poStdout `disjoint` poStderr
                    let isBinary = not $ any isDestString dest && not (any isDestBytes dest)
                    when isTied $ hSetBuffering h LineBuffering
                    when (DestEcho `elem` dest) $ do
                        buf <- hGetBuffering hh
                        case buf of
                            BlockBuffering{} -> return ()
                            _ -> hSetBuffering h buf

                    if isBinary then do
                        dest <- return $ for dest $ \d -> case d of
                            DestEcho -> BS.hPut hh
                            DestFile x -> BS.hPut (fileHandle x)
                            DestString x -> addBuffer x . (if isWindows then replace "\r\n" "\n" else id) . BS.unpack
                            DestBytes x -> addBuffer x
                        forkWait $ whileM $ do
                            src <- BS.hGetSome h 4096
                            mapM_ ($ src) dest
                            notM $ hIsEOF h
                     else do
                        src <- hGetContents h
                        wait1 <- forkWait $ C.evaluate $ rnf src
                        waits <- forM dest $ \d -> case d of
                            DestEcho -> forkWait $ hPutStr hh src
                            DestFile x -> forkWait $ hPutStr (fileHandle x) src
                            DestString x ->
                                if all (DestString x `elem`) [poStdout, poStderr] then
                                    forkWait $ mapM_ (addBuffer x . return) src
                                else do
                                    addBuffer x src
                                    return $ return ()
                        return $ sequence_ $ wait1 : waits

                whenJust inh $ snd $ stdIn poStdin
                sequence_ wait
                flushBuffers
                res <- waitForProcess pid
                whenJust outh hClose
                whenJust errh hClose
                return res
