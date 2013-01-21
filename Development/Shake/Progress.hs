{-# LANGUAGE DeriveDataTypeable, RecordWildCards, CPP, ForeignFunctionInterface #-}

-- | Progress tracking
module Development.Shake.Progress(
    progressSimple, progressDisplay, progressTitlebar,
    -- * More advanced functions
    Progress(..),
    progressDone, progressTodo, progressFraction,
    progressScale, progressSample
    ) where

import Control.Concurrent
import Data.Data
import Data.IORef
import Data.List
import qualified Data.ByteString.Char8 as BS

#ifdef mingw32_HOST_OS

import Foreign
import Foreign.C.Types

type LPCSTR = Ptr CChar

foreign import stdcall "Windows.h SetConsoleTitleA" c_setConsoleTitle :: LPCSTR -> IO Bool

#endif


-- | Information about the current state of the build, obtained by passing a callback function
--   to 'shakeProgress'. Typically a program will poll this value to provide progress messages.
--   The following example displays the approximate single-threaded time remaining
--   as the console title.
--
-- @
--showProgress :: IO 'Progress' -> IO ()
--showProgress progress = void $ forkIO loop
--    where loop = do
--        current <- progress
--        when ('isRunning' current) $ do
--            let (s,c) = timeTodo current
--            setTitle $ \"Todo = \" ++ show (ceiling s) ++ \"s (+ \" ++ show c ++ \" unknown)\"
--            threadDelay $ 5 * 1000000
--            loop
--
--setTitle :: String -> IO ()
--setTitle s = putStr $ \"\\ESC]0;\" ++ s ++ \"\\BEL\"
-- @
data Progress = Progress
    {isRunning :: !Bool -- ^ Starts out 'True', becomes 'False' once the build has completed.
    ,countSkipped :: {-# UNPACK #-} !Int -- ^ Number of rules which were required, but were already in a valid state.
    ,countBuilt :: {-# UNPACK #-} !Int -- ^ Number of rules which were have been built in this run.
    ,countUnknown :: {-# UNPACK #-} !Int -- ^ Number of rules which have been built previously, but are not yet known to be required.
    ,countTodo :: {-# UNPACK #-} !Int -- ^ Number of rules which are currently required (ignoring dependencies that do not change), but not built.
    ,timeSkipped :: {-# UNPACK #-} !Double -- ^ Time spent building 'countSkipped' rules in previous runs.
    ,timeBuilt :: {-# UNPACK #-} !Double -- ^ Time spent building 'countBuilt' rules.
    ,timeUnknown :: {-# UNPACK #-} !Double -- ^ Time spent building 'countUnknown' rules in previous runs.
    ,timeTodo :: {-# UNPACK #-} !(Double,Int) -- ^ Time spent building 'countTodo' rules in previous runs, plus the number which have no known time (have never been built before).
    }
    deriving (Eq,Ord,Show,Data,Typeable)


-- | Scale to aim to hit 0 at the same time as the original, but reducing by 1 each time.
--   The current implementation uses interquartile mean of the deltas over a window of 100.
--   A result of 'Nothing' is given if it thinks the system will never terminate.
progressScale :: IO (Double -> IO (Maybe Double))
progressScale = do
    vals <- newIORef []
    return $ \v -> do
        vs <- fmap ((v:) . take 100) $ readIORef vals
        writeIORef vals vs
        let xs = sort $ zipWith (-) vs $ tail vs
            n = length xs `div` 4
            xs2 = drop n $ reverse $ drop n xs
            avg = genericLength xs2 / sum xs2
        return $ if avg <= 0 then Nothing else Just $ v / avg


-- | Time left todo, would be correct if building on one processor with the same performance as last time
progressTodo :: Progress -> Double
progressTodo Progress{..} =
        fst timeTodo + (if avgSamples == 0 || snd timeTodo == 0 then 0 else fromIntegral (snd timeTodo) * avgTime)
    where
        avgTime = (timeBuilt + fst timeTodo) / fromIntegral avgSamples
        avgSamples = countBuilt + countTodo - snd timeTodo

-- | Time completed, would be correct if building on one processor with the same performance as last time.
--   Includes time from rules that were skipped.
progressDone :: Progress -> Double
progressDone Progress{..} = timeSkipped + timeBuilt


progressFraction :: Progress -> Double
progressFraction p = if b == 0 then 0 else t / b
    where t = progressTodo p
          b = t + progressDone p

-- | This function is asyncronous
progressSample :: IO Progress -> Double -> (Maybe Progress -> IO ()) -> IO ()
progressSample ask wait notify = do
    threadDelay $ ceiling $ wait * 1000000
    p <- ask
    if not $ isRunning p then
        notify Nothing
     else do
        notify $ Just p
        progressSample ask wait notify


progressDisplay :: (String -> IO ()) -> IO Progress -> IO ()
progressDisplay disp ask = do
    scale <- progressScale
    progressSample ask 5 $ \p -> case p of
        Nothing -> disp "Finished"
        Just p -> do
            time <- fmap (fmap (*5)) $ scale $ progressTodo p
            disp $ maybe "?:??" (showSecs . ceiling) time ++ "m (" ++ show (ceiling $ progressFraction p * 100) ++ "%)"
    where
        showSecs i = show mins ++ ":" ++ ['0' | secs < 10] ++ show secs
            where (mins,secs) = divMod i (60 :: Int)


-- | Set the title bar.
progressTitlebar :: String -> IO ()
progressTitlebar x =
#ifdef mingw32_HOST_OS
    BS.useAsCString (BS.pack x) $ \x -> c_setConsoleTitle x >> return ()
#else
    BS.putStr $ BS.pack $ "\ESC]0;" ++ x ++ "\BEL"
#endif


-- | Simple function, sets the titlebar to the current progress every 5 seconds.
--   Displays the time in both 
progressSimple :: IO Progress -> IO ()
progressSimple = progressDisplay progressTitlebar
