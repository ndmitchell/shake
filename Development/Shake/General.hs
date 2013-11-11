{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, CPP #-}

module Development.Shake.General(
    Lock, newLock, withLock, withLockTry,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier,
    Duration, duration, Time, offsetTime, sleep,
    isWindows,
    modifyIORef'', writeIORef'',
    whenJust, loop, whileM,
    fastNub, showQuote,
    BS, pack, unpack, pack_, unpack_,
    BSU, packU, unpackU, packU_, unpackU_, requireU
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.Time
import qualified Data.ByteString as BS (any)
import qualified Data.ByteString.Char8 as BS hiding (any)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashSet as Set
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


---------------------------------------------------------------------
-- System.Info

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif


---------------------------------------------------------------------
-- Data.ByteString
-- Mostly because ByteString does not have an NFData instance in older GHC

-- | ASCII ByteString
newtype BS = BS BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BS where
    -- some versions of ByteString do not have NFData instances, but seq is equivalent
    -- for a strict bytestring. Therefore, we write our own instance.
    rnf (BS x) = x `seq` ()


-- | UTF8 ByteString
newtype BSU = BSU BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BSU where
    rnf (BSU x) = x `seq` ()



pack :: String -> BS
pack = pack_ . BS.pack

unpack :: BS -> String
unpack = BS.unpack . unpack_

pack_ :: BS.ByteString -> BS
pack_ = BS

unpack_ :: BS -> BS.ByteString
unpack_ (BS x) = x

packU :: String -> BSU
packU = packU_ . UTF8.fromString

unpackU :: BSU -> String
unpackU = UTF8.toString . unpackU_

unpackU_ :: BSU -> BS.ByteString
unpackU_ (BSU x) = x

packU_ :: BS.ByteString -> BSU
packU_ = BSU

requireU :: BSU -> Bool
requireU = BS.any (>= 0x80) . unpackU_
