{-# LANGUAGE BangPatterns, CPP #-}

module General.Base(
    Duration, duration, Time, diffTime, offsetTime, offsetTimeIncrease, sleep,
    getProcessorCount,
    randomElem,
    showTime,
    fastNub, showQuote, word1
    ) where

import Data.Tuple.Extra
import Control.Concurrent
import Control.Exception.Extra
import Data.Char
import Data.IORef
import Data.List
import Data.Time
import qualified Data.HashSet as Set
import Numeric.Extra
import System.Environment.Extra
import System.IO.Unsafe
import System.Random
import Development.Shake.Classes
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types
#endif


---------------------------------------------------------------------
-- Data.Time

type Time = Double -- how far you are through this run, in seconds

diffTime :: UTCTime -> UTCTime -> Duration
diffTime end start = fromRational $ toRational $ end `diffUTCTime` start

-- | Call once at the start, then call repeatedly to get Time values out
offsetTime :: IO (IO Time)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ diffTime end start

-- | Like offsetTime, but results will never decrease (though they may stay the same)
offsetTimeIncrease :: IO (IO Time)
offsetTimeIncrease = do
    t <- offsetTime
    ref <- newIORef 0
    return $ do
        t <- t
        atomicModifyIORef ref $ \o -> let m = max t o in m `seq` (m, m)


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

showTime :: Double -> String
showTime x | x >= 3600 = f (x / 60) "h" "m"
           | x >= 60 = f x "m" "s"
           | otherwise = showDP 2 x ++ "s"
    where
        f x m s = show ms ++ m ++ ['0' | ss < 10] ++ show ss ++ s
            where (ms,ss) = round x `divMod` 60


---------------------------------------------------------------------
-- System.Info

#if __GLASGOW_HASKELL__ >= 704
-- Use the underlying GHC function
foreign import ccall getNumberOfProcessors :: IO CInt
#endif


{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
#if __GLASGOW_HASKELL__ >= 704
            if rtsSupportsBoundThreads then
                fmap fromIntegral $ getNumberOfProcessors
            else
#endif
                handle_ (const $ return 1) $ do
                    env <- lookupEnv "NUMBER_OF_PROCESSORS"
                    case env of
                        Just s | [(i,"")] <- reads s -> return i
                        _ -> do
                            src <- readFile "/proc/cpuinfo"
                            return $ length [() | x <- lines src, "processor" `isPrefixOf` x]


---------------------------------------------------------------------
-- System.Random

randomElem :: [a] -> IO a
randomElem xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i
