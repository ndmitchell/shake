
module Development.Shake.Timing(resetTimings, addTiming, printTimings) where

import Control.Arrow
import Data.IORef
import Data.Time
import System.IO.Unsafe
import Numeric


{-# NOINLINE timings #-}
timings :: IORef [(UTCTime, String)] -- number of times called, newest first
timings = unsafePerformIO $ newIORef []


resetTimings :: IO ()
resetTimings = do
    now <- getCurrentTime
    writeIORef timings [(now, "Start")]


-- | Print all withTiming information and clear the information.
printTimings :: IO ()
printTimings = do
    now <- getCurrentTime
    old <- atomicModifyIORef timings $ \ts -> ([(now, "Start")], ts)
    putStr $ unlines $ showTimings now $ reverse old


addTiming :: String -> IO ()
addTiming msg = do
    now <- getCurrentTime
    atomicModifyIORef timings $ \ts -> ((now,msg):ts, ())


showTimings :: UTCTime -> [(UTCTime, String)] -> [String]
showTimings stop times = map (\(a,b) -> a ++ " " ++ showDP 3 b) xs
    where xs = [ (name,fromRational $ toRational $ stop `diffUTCTime` start)
               | ((start, name), stop) <- zip times $ map fst (drop 1 times) ++ [stop]]


showDP :: Int -> Double -> String
showDP n x = a ++ "." ++ b ++ replicate (n - length b) '0'
    where (a,b) = second (drop 1) $ break (== '.') $ showFFloat (Just 2) x ""
