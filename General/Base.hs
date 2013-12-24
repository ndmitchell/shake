{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}

module General.Base(
    Lock, newLock, withLock, withLockTry,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier,
    Duration, duration, Time, offsetTime, sleep,
    isWindows, getProcessorCount,
    readFileUCS2, getEnvMaybe,
    modifyIORef'', writeIORef'',
    whenJust, loop, whileM, partitionM, concatMapM, mapMaybeM,
    fastNub, showQuote,
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.HashSet as Set
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Unsafe
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


---------------------------------------------------------------------
-- Data.Time

type Time = Double -- how far you are through this run, in seconds

-- | Call once at the start, then call repeatedly to get Time values out
offsetTime :: IO (IO Time)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ fromRational $ toRational $ end `diffUTCTime` start


type Duration = Double -- duration in seconds

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


---------------------------------------------------------------------
-- Control.Monad

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing f = return ()

loop :: Monad m => (a -> m (Either a b)) -> a -> m b
loop act x = do
    res <- act x
    case res of
        Left x -> loop act x
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
