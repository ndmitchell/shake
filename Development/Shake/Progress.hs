{-# LANGUAGE DeriveDataTypeable, RecordWildCards, CPP, ForeignFunctionInterface #-}

-- | Progress tracking
module Development.Shake.Progress(
    Progress(..),
    progressSimple, progressDisplay, progressTitlebar,
    ) where

import Control.Concurrent
import Data.Data
import Data.Monoid
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

instance Monoid Progress where
    mempty = Progress True 0 0 0 0 0 0 0 (0,0)
    mappend a b = Progress
        {isRunning = isRunning a && isRunning b
        ,countSkipped = countSkipped a + countSkipped b
        ,countBuilt = countBuilt a + countBuilt b
        ,countUnknown = countUnknown a + countUnknown b
        ,countTodo = countTodo a + countTodo b
        ,timeSkipped = timeSkipped a + timeSkipped b
        ,timeBuilt = timeBuilt a + timeBuilt b
        ,timeUnknown = timeUnknown a + timeUnknown b
        ,timeTodo = let (a1,a2) = timeTodo a; (b1,b2) = timeTodo b
                        x1 = a1 + b1; x2 = a2 + b2
                    in x1 `seq` x2 `seq` (x1,x2)
        }

-- Including timeSkipped gives a more truthful percent, but it drops more sharply
-- which isn't what users probably want
progressDone :: Progress -> Double
progressDone Progress{..} = timeBuilt


-- | Make a guess at the number of seconds to go, ignoring multiple threads
progressTodo :: Progress -> Double
progressTodo Progress{..} =
        fst timeTodo + (if avgSamples == 0 || snd timeTodo == 0 then 0 else fromIntegral (snd timeTodo) * avgTime)
    where
        avgTime = (timeBuilt + fst timeTodo) / fromIntegral avgSamples
        avgSamples = countBuilt + countTodo - snd timeTodo


progressDisplay :: Double -> (String -> IO ()) -> IO Progress -> IO ()
progressDisplay sample disp prog = loop 0
    where
        loop steps = do
            p <- prog
            if not $ isRunning p then
                disp "Finished"
             else do
                disp $ if steps == 0
                    then "Starting..."
                    else let done = progressDone p
                             todo = progressTodo p
                             comp = if done == 0 then todo else sample * todo / (done / fromIntegral steps)
                             (mins,secs) = divMod (ceiling comp) (60 :: Int)
                             time = show mins ++ ":" ++ ['0' | secs < 10] ++ show secs
                             perc = show (floor (if done == 0 then 0 else 100 * done / (done + todo)) :: Int)
                         in time ++ "m (" ++ perc ++ "%)" ++ "\a"
                threadDelay $ ceiling $ sample * 1000000
                loop $! steps+1


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
progressSimple = progressDisplay 5 progressTitlebar
