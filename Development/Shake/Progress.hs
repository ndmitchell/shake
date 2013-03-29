{-# LANGUAGE DeriveDataTypeable, RecordWildCards, CPP, ForeignFunctionInterface, ScopedTypeVariables #-}

-- | Progress tracking
module Development.Shake.Progress(
    Progress(..),
    progressSimple, progressDisplay, progressTitlebar,
    progressDisplayTester -- INTERNAL FOR TESTING ONLY
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
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
--   Typical status messages will take the form of @1m25s (15%)@, indicating that the build
--   is predicted to complete in 1 minute 25 seconds (85 seconds total), and 15% of the necessary build time has elapsed.
--   This function uses past observations to predict future behaviour, and as such, is only
--   guessing. The time is likely to go up as well as down, and will be less accurate from a
--   clean build (as the system has fewer past observations).
--
--   The current implementation is to predict the time remaining (based on 'timeTodo') and the
--   work already done ('timeBuilt'). The percentage is then calculated as @remaining / (done + remaining)@,
--   while time left is calculated by scaling @remaining@ by the observed work rate in this build,
--   roughly @done / time_elapsed@.
progressDisplay :: Double -> (String -> IO ()) -> IO Progress -> IO ()
progressDisplay = progressDisplayer True


-- | Version of 'progressDisplay' that omits the sleep
progressDisplayTester :: Double -> (String -> IO ()) -> IO Progress -> IO ()
progressDisplayTester = progressDisplayer False


progressDisplayer :: Bool -> Double -> (String -> IO ()) -> IO Progress -> IO ()
progressDisplayer sleep sample disp prog = do
    disp "Starting..." -- no useful info at this stage
    loop $ tick0 sample
    where
        loop :: Tick -> IO ()
        loop t = do
            when sleep $ threadDelay $ ceiling $ sample * 1000000
            p <- prog
            if not $ isRunning p then
                disp "Finished"
             else do
                (t, msg) <- return $ tick p t
                disp msg
                loop $! t


-- work_ and done_ are both as recorded at step_
data Tick = Tick
    {sample :: {-# UNPACK #-} !Double
    ,step :: {-# UNPACK #-} !Double
    ,work_ :: {-# UNPACK #-} !Double
    ,done_ :: {-# UNPACK #-} !Double
    ,step_ :: {-# UNPACK #-} !Double
    } deriving Show

tick0 :: Double -> Tick
tick0 sample = Tick sample 0 0 0 0

-- How much additional weight to give to the latest work values
factor :: Double
factor = 1.2

tick :: Progress -> Tick -> (Tick, String)
tick p tickOld@Tick{..}
        | done == 0 = (tick, display 1) -- no scaling, or we'd divide by zero
        | done == done_ = (tick, display work_) -- use the last work rate
        | otherwise = (tick{work_=newWork, done_=done, step_=step'}, display newWork)
    where
        step' = step+sample
        tick = tickOld{step=step'}
        done = progressDone p
        todo = progressTodo p

        newWork = ((step_ * work_) + ((done - done_) * factor)) /
                  ((step_ + ((step' - step_) * factor)))

        display work = time ++ "s (" ++ perc ++ "%)"
            where guess = todo / work
                  (mins,secs) = divMod (ceiling guess) (60 :: Int)
                  time = (if mins == 0 then "" else show mins ++ "m" ++ ['0' | secs < 10]) ++ show secs
                  perc = show (floor (if done == 0 then 0 else 100 * done / (done + todo)) :: Int)


{-# NOINLINE xterm #-}
xterm :: Bool
xterm = System.IO.Unsafe.unsafePerformIO $
    Control.Exception.catch (fmap (== "xterm") $ getEnv "TERM") $
    \(e :: SomeException) -> return False


-- | Set the title of the current console window to the given text. If the
--   environment variable @$TERM@ is set to @xterm@ this uses xterm escape sequences.
--   On Windows, if not detected as an xterm, this function uses the @SetConsoleTitle@ API.
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
