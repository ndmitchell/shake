{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}

module General.Base(
    Lock, newLock, withLock, withLockTry,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    Duration, duration, Time, offsetTime, sleep,
    isWindows, getProcessorCount,
    readFileUCS2, getEnvMaybe, captureOutput, getExePath,
    showDP, showTime,
    modifyIORef'', writeIORef'',
    whenJust, loopM, whileM, partitionM, concatMapM, mapMaybeM, liftA2',
    fastNub, showQuote, word1,
    withBufferMode, withCapabilities
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.HashSet as Set
import Numeric
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Unsafe
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import Development.Shake.Classes


---------------------------------------------------------------------
-- LOCK

-- | Like an MVar, but has no value
newtype Lock = Lock (MVar ())
instance Show Lock where show _ = "Lock"

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const

withLockTry :: Lock -> IO a -> IO (Maybe a)
withLockTry (Lock m) act =
    mask $ \restore -> do
        a <- tryTakeMVar m
        case a of
            Nothing -> return Nothing
            Just _ -> restore (fmap Just act) `finally` putMVar m ()


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)
instance Show (Var a) where show _ = "Var"

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

withVar :: Var a -> (a -> IO b) -> IO b
withVar (Var x) f = withMVar x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once
newtype Barrier a = Barrier (MVar a)
instance Show (Barrier a) where show _ = "Barrier"

newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = putMVar x

waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x

waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier x) = do
    res <- tryTakeMVar x
    whenJust res $ putMVar x
    return res


---------------------------------------------------------------------
-- Data.Time

type Time = Float -- how far you are through this run, in seconds

-- | Call once at the start, then call repeatedly to get Time values out
offsetTime :: IO (IO Time)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ fromRational $ toRational $ end `diffUTCTime` start


type Duration = Float -- duration in seconds

duration :: IO a -> IO (Duration, a)
duration act = do
    time <- offsetTime
    res <- act
    time <- time
    return (time, res)


sleep :: Duration -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


---------------------------------------------------------------------
-- Data.IORef

-- Two 's because GHC 7.6 has a strict modifyIORef
modifyIORef'' :: IORef a -> (a -> a) -> IO ()
modifyIORef'' ref f = do
    x <- readIORef ref
    writeIORef'' ref $ f x

writeIORef'' :: IORef a -> a -> IO ()
writeIORef'' ref !x = writeIORef ref x


---------------------------------------------------------------------
-- Data.List

-- | Like 'nub', but the results may be in any order.
fastNub :: (Eq a, Hashable a) => [a] -> [a]
fastNub = f Set.empty
    where f seen [] = []
          f seen (x:xs) | x `Set.member` seen = f seen xs
                        | otherwise = x : f (Set.insert x seen) xs


showQuote :: String -> String
showQuote xs | any isSpace xs = "\"" ++ concatMap (\x -> if x == '\"' then "\"\"" else [x]) xs ++ "\""
             | otherwise = xs


word1 :: String -> (String, String)
word1 x = second (dropWhile isSpace) $ break isSpace $ dropWhile isSpace x


---------------------------------------------------------------------
-- Data.String

showDP :: Int -> Double -> String
showDP n x = a ++ "." ++ b ++ replicate (n - length b) '0'
    where (a,b) = second (drop 1) $ break (== '.') $ showFFloat (Just n) x ""

showTime :: Double -> String
showTime x | x >= 3600 = f (x / 60) "h" "m"
           | x >= 60 = f x "m" "s"
           | otherwise = showDP 2 x ++ "s"
    where
        f x m s = show ms ++ m ++ ['0' | ss < 10] ++ show ss ++ m
            where (ms,ss) = round x `divMod` 60


---------------------------------------------------------------------
-- Control.Monad

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing f = return ()

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> return v

whileM :: Monad m => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    t <- f x
    (a,b) <- partitionM f xs
    return $ if t then (x:a,b) else (a,x:b)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = liftM catMaybes $ mapM f xs

liftA2' :: Applicative m => m a -> m b -> (a -> b -> c) -> m c
liftA2' a b f = liftA2 f a b


---------------------------------------------------------------------
-- System.Info

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

-- Could be written better in C, but sticking to Haskell for laziness
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act = handle (\(_ :: SomeException) -> return 1) $ do
            env <- getEnvMaybe "NUMBER_OF_PROCESSORS"
            case env of
                Just s | [(i,"")] <- reads s -> return i
                _ -> do
                    src <- readFile "/proc/cpuinfo"
                    return $ length [() | x <- lines src, "processor" `isPrefixOf` x]


---------------------------------------------------------------------
-- System.IO

readFileUCS2 :: FilePath -> IO String
readFileUCS2 name = openFile name ReadMode >>= \h -> do
    hSetEncoding h utf16
    hGetContents h

getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe x = catchJust (\x -> if isDoesNotExistError x then Just x else Nothing) (fmap Just $ getEnv x) (const $ return Nothing)

captureOutput :: IO () -> IO String
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile f
    evaluate $ length res
    removeFile f
    return res

withCapabilities :: Int -> IO a -> IO a
#if __GLASGOW_HASKELL__ >= 706
withCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act
#endif
withCapabilities new act = act

withBufferMode :: Handle -> BufferMode -> IO a -> IO a
withBufferMode h b act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h LineBuffering
    act


getExePath :: IO FilePath
#if __GLASGOW_HASKELL__ >= 706
getExePath = getExecutablePath
#else
getExePath = getProgName
#endif
