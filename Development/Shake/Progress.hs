{-# LANGUAGE DeriveDataTypeable #-}

-- | Progress tracking
module Development.Shake.Progress(
    Progress(..)
    ) where

import Data.Data


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
