{-# LANGUAGE DeriveDataTypeable, RecordWildCards, CPP, ForeignFunctionInterface, ScopedTypeVariables #-}

-- | Progress tracking
module Development.Shake.Progress(
    Progress(..),
    progressSimple, progressDisplay, progressTitlebar,
    ) where

import Control.Concurrent
import Control.Exception
import System.Environment
import Data.Data
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe

#ifdef mingw32_HOST_OS

import Foreign
import Foreign.C.Types

type LPCSTR = Ptr CChar

foreign import stdcall "Windows.h SetConsoleTitleA" c_setConsoleTitle :: LPCSTR -> IO Bool

#endif


-- | Information about the current state of the build, obtained by passing a callback function
--   to 'Development.Shake.shakeProgress'. Typically a program will use 'progressDisplay' to poll this value and produce
--   status messages, which is implemented using this data type.
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


-- | Given a sampling interval (in seconds) and a way to display the status message,
--   produce a function suitable for using as 'Development.Shake.shakeProgress'.
--   This function polls the progress information every /n/ seconds, produces a status
--   message and displays it using the display function.
--
--   Typical status messages will take the form of @1:25m (15%)@, indicating that the build
--   is predicted to complete in 1min 25sec, and 15% of the necessary build time has elapsed.
--   This function uses past observations to predict future behaviour, and as such, is only
--   guessing. The time is likely to go up as well as down, and will be less accurate from a
--   clean build (as the system has fewer past observations).
--
--   The current implementation is to predict the time remaining (based on 'timeTodo') and the
--   work already done ('timeBuilt'). The percentage is then calculated as @remaining / (done + remaining)@,
--   while time left is calculated by scaling @remaining@ by the observed work rate in this build,
--   namely @done / time_elapsed@.
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
                             time = (if mins == 0 then "" else show mins ++ "m" ++ ['0' | secs < 10]) ++ show secs
                             perc = show (floor (if done == 0 then 0 else 100 * done / (done + todo)) :: Int)
                         in time ++ "s (" ++ perc ++ "%)"
                threadDelay $ ceiling $ sample * 1000000
                loop $! steps+1


{-# NOINLINE xterm #-}
xterm :: Bool
xterm = System.IO.Unsafe.unsafePerformIO $
    Control.Exception.catch (fmap (== "xterm") $ getEnv "TERM") $
    \(e :: SomeException) -> return False



-- | Set the title of the current console window to the given text. On Windows
--   this function uses the @SetConsoleTitle@ API, elsewhere it uses an xterm
--   escape sequence. This function may not work for all terminals.
progressTitlebar :: String -> IO ()
progressTitlebar x
    | xterm = BS.putStr $ BS.pack $ "\ESC]0;" ++ x ++ "\BEL"
#ifdef mingw32_HOST_OS
    | otherwise = BS.useAsCString (BS.pack x) $ \x -> c_setConsoleTitle x >> return ()
#else
    | otherwise = return ()
#endif


-- | A simple method for displaying progress messages, suitable for using as
--   'Development.Shake.shakeProgress'. This function writes the current progress to
--   the titlebar every five seconds. The function is defined as:
--
-- @
--progressSimple = 'progressDisplay' 5 'progressTitlebar'
-- @
progressSimple :: IO Progress -> IO ()
progressSimple = progressDisplay 5 progressTitlebar
