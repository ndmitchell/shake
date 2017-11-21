{-# LANGUAGE DeriveDataTypeable, RecordWildCards, CPP, ViewPatterns, ForeignFunctionInterface #-}

-- | Progress tracking
module Development.Shake.Internal.Progress(
    Progress(..),
    progressSimple, progressDisplay, progressTitlebar, progressProgram,
    ProgressEntry(..), progressReplay, writeProgressReport -- INTERNAL USE ONLY
    ) where

import Control.Applicative
import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad
import System.Environment.Extra
import System.Directory
import System.Process
import System.FilePath
import Data.Char
import Data.Data
import Data.IORef
import Data.List
import Data.Maybe
import Data.Version
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Numeric.Extra
import General.Template
import System.IO.Unsafe
import Paths_shake
import System.Time.Extra
import Data.Monoid
import Prelude

#ifdef mingw32_HOST_OS

import Foreign
import Foreign.C.Types

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV "Windows.h SetConsoleTitleA" c_setConsoleTitle :: Ptr CChar -> IO Bool

#endif


---------------------------------------------------------------------
-- PROGRESS TYPES - exposed to the user

-- | Information about the current state of the build, obtained by either passing a callback function
--   to 'Development.Shake.shakeProgress' (asynchronous output) or 'Development.Shake.getProgress'
--   (synchronous output). Typically a build system will pass 'progressDisplay' to 'Development.Shake.shakeProgress',
--   which will poll this value and produce status messages.
data Progress = Progress
    {isFailure :: !(Maybe String) -- ^ Starts out 'Nothing', becomes 'Just' a target name if a rule fails.
    ,countSkipped :: {-# UNPACK #-} !Int -- ^ Number of rules which were required, but were already in a valid state.
    ,countBuilt :: {-# UNPACK #-} !Int -- ^ Number of rules which were have been built in this run.
    ,countUnknown :: {-# UNPACK #-} !Int -- ^ Number of rules which have been built previously, but are not yet known to be required.
    ,countTodo :: {-# UNPACK #-} !Int -- ^ Number of rules which are currently required (ignoring dependencies that do not change), but not built.
    ,timeSkipped :: {-# UNPACK #-} !Double -- ^ Time spent building 'countSkipped' rules in previous runs.
    ,timeBuilt :: {-# UNPACK #-} !Double -- ^ Time spent building 'countBuilt' rules.
    ,timeUnknown :: {-# UNPACK #-} !Double -- ^ Time spent building 'countUnknown' rules in previous runs.
    ,timeTodo :: {-# UNPACK #-} !(Double,Int) -- ^ Time spent building 'countTodo' rules in previous runs, plus the number which have no known time (have never been built before).
    }
    deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Semigroup Progress where
    (<>) = mappend

instance Monoid Progress where
    mempty = Progress Nothing 0 0 0 0 0 0 0 (0,0)
    mappend a b = Progress
        {isFailure = isFailure a `mplus` isFailure b
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


---------------------------------------------------------------------
-- MEALY TYPE - for writing the progress functions
-- See <http://hackage.haskell.org/package/machines-0.2.3.1/docs/Data-Machine-Mealy.html>

-- | A machine that takes inputs and produces outputs
newtype Mealy i a = Mealy {runMealy :: i -> (a, Mealy i a)}

instance Functor (Mealy i) where
    fmap f (Mealy m) = Mealy $ \i -> case m i of
        (x, m) -> (f x, fmap f m)

instance Applicative (Mealy i) where
    pure x = let r = Mealy (const (x, r)) in r
    Mealy mf <*> Mealy mx = Mealy $ \i -> case mf i of
        (f, mf) -> case mx i of
            (x, mx) -> (f x, mf <*> mx)

echoMealy :: Mealy i i
echoMealy = Mealy $ \i -> (i, echoMealy)

scanMealy :: (a -> b -> a) -> a -> Mealy i b -> Mealy i a
scanMealy f z (Mealy m) = Mealy $ \i -> case m i of
    (x, m) -> let z2 = f z x in (z2, scanMealy f z2 m)


---------------------------------------------------------------------
-- MEALY UTILITIES

oldMealy :: a -> Mealy i a -> Mealy i (a,a)
oldMealy old = scanMealy (\(_,old) new -> (old,new)) (old,old)

latch :: Mealy i (Bool, a) -> Mealy i a
latch s = fromJust <$> scanMealy f Nothing s
    where f old (b,v) = Just $ if b then fromMaybe v old else v

iff :: Mealy i Bool -> Mealy i a -> Mealy i a -> Mealy i a
iff c t f = (\c t f -> if c then t else f) <$> c <*> t <*> f

-- decay'd division, compute a/b, with a decay of f
-- r' is the new result, r is the last result
-- r' ~= a' / b'
-- r' = r*b + f*(a'-a)
--      -------------
--      b + f*(b'-b)
-- when f == 1, r == r'
--
-- both streams must only ever increase
decay :: Double -> Mealy i Double -> Mealy i Double -> Mealy i Double
decay f a b = scanMealy step 0 $ (,) <$> oldMealy 0 a <*> oldMealy 0 b
    where step r ((a,a'),(b,b')) = if isNaN r then a' / b' else ((r*b) + f*(a'-a)) / (b + f*(b'-b))


---------------------------------------------------------------------
-- MESSAGE GENERATOR

formatMessage :: Double -> Double -> String
formatMessage secs perc =
    (if isNaN secs || secs < 0 then "??s" else showMinSec $ ceiling secs) ++ " (" ++
    (if isNaN perc || perc < 0 || perc > 100 then "??" else show $ floor perc) ++ "%)"

showMinSec :: Int -> String
showMinSec secs = (if m == 0 then "" else show m ++ "m" ++ ['0' | s < 10]) ++ show s ++ "s"
    where (m,s) = divMod secs 60

liftA2' :: Applicative m => m a -> m b -> (a -> b -> c) -> m c
liftA2' a b f = liftA2 f a b


-- | return (number of seconds, percentage, explanation)
message :: Mealy (Double, Progress) (Double, Progress) -> Mealy (Double, Progress) (Double, Double, String)
message input = liftA3 (,,) time perc debug
    where
        progress = snd <$> input
        secs = fst <$> input
        debug = (\donePerSec ruleTime (todoKnown,todoUnknown) ->
            "Progress: " ++
                "((known=" ++ showDP 2 todoKnown ++ "s) + " ++
                "(unknown=" ++ show todoUnknown ++ " * time=" ++ showDP 2 ruleTime ++ "s)) " ++
                "(rate=" ++ showDP 2 donePerSec ++ "))")
            <$> donePerSec <*> ruleTime <*> (timeTodo <$> progress)

        -- Number of seconds work completed in this build run
        -- Ignores timeSkipped which would be more truthful, but it makes the % drop sharply
        -- which isn't what users want
        done = timeBuilt <$> progress

        -- Work done per second, don't divide by 0 and don't update if 'done' doesn't change
        donePerSec = iff ((==) 0 <$> done) (pure 1) perSecStable
            where perSecStable = latch $ liftA2 (,) (uncurry (==) <$> oldMealy 0 done) perSecRaw
                  perSecRaw = decay 1.2 done secs

        -- Predicted build time for a rule that has never been built before
        -- The high decay means if a build goes in "phases" - lots of source files, then lots of compiling
        -- we reach a reasonable number fairly quickly, without bouncing too much
        ruleTime = liftA2 weightedAverage
            (f (decay 10) timeBuilt countBuilt)
            (f (liftA2 (/)) (fst . timeTodo) (\Progress{..} -> countTodo - snd timeTodo))
            -- don't call decay on todo, since it goes up and down (as things get done)
            where
                weightedAverage (w1,x1) (w2,x2)
                    | w1 == 0 && w2 == 0 = 0
                    | otherwise = ((w1 *. x1) + (w2 *. x2)) / intToDouble (w1+w2)
                    where i *. d = if i == 0 then 0 else intToDouble i * d -- since d might be NaN

                f divide time count = let xs = count <$> progress in liftA2 (,) xs $ divide (time <$> progress) (intToDouble <$> xs)

        -- Number of seconds work remaining, ignoring multiple threads
        todo = f <$> progress <*> ruleTime
            where f Progress{..} ruleTime = fst timeTodo + (fromIntegral (snd timeTodo) * ruleTime)

        -- Display information
        time = liftA2 (/) todo donePerSec
        perc = iff ((==) 0 <$> done) (pure 0) $
            liftA2' done todo $ \done todo -> 100 * done / (done + todo)


---------------------------------------------------------------------
-- EXPOSED FUNCTIONS

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
progressDisplay sample disp prog = do
    disp "Starting..." -- no useful info at this stage
    time <- offsetTime
    catchJust (\x -> if x == ThreadKilled then Just () else Nothing) (loop time $ message echoMealy) (const $ disp "Finished")
    where
        loop :: IO Double -> Mealy (Double, Progress) (Double, Double, String) -> IO ()
        loop time mealy = do
            sleep sample
            p <- prog
            t <- time
            ((secs,perc,debug), mealy) <- return $ runMealy mealy (t, p)
            -- putStrLn debug
            disp $ formatMessage secs perc ++ maybe "" (\err -> ", Failure! " ++ err) (isFailure p)
            loop time mealy


data ProgressEntry = ProgressEntry
    {idealSecs :: Double, idealPerc :: Double
    ,actualSecs :: Double, actualPerc :: Double
    }

isInvalid :: ProgressEntry -> Bool
isInvalid ProgressEntry{..} = isNaN actualSecs || isNaN actualPerc


-- | Given a list of progress inputs, what would you have suggested (seconds, percentage)
progressReplay :: [(Double, Progress)] -> [ProgressEntry]
progressReplay [] = []
progressReplay ps = snd $ mapAccumL f (message echoMealy) ps
    where
        end = fst $ last ps
        f a (time,p) = (a2, ProgressEntry (end - time) (time * 100 / end) secs perc)
            where ((secs,perc,_),a2) = runMealy a (time,p)


-- | Given a trace, display information about how well we did
writeProgressReport :: FilePath -> [(FilePath, [(Double, Progress)])] -> IO ()
writeProgressReport out (map (second progressReplay) -> xs)
    | (bad,_):_ <- filter (any isInvalid . snd) xs = errorIO $ "Progress generates NaN for " ++ bad
    | takeExtension out == ".js" = writeFile out $ "var shake = \n" ++ generateJSON xs
    | takeExtension out == ".json" = writeFile out $ generateJSON xs
    | out == "-" = putStr $ unlines $ generateSummary xs
    | otherwise = LBS.writeFile out =<< generateHTML xs


generateSummary :: [(FilePath, [ProgressEntry])] -> [String]
generateSummary xs = flip concatMap xs $ \(file,xs) ->
    ["# " ++ file, f xs "Seconds" idealSecs actualSecs, f xs "Percent" idealPerc actualPerc]
    where
        levels = [100,90,80,50]
        f xs lbl ideal actual = lbl ++ ": " ++ intercalate ", "
            [show l ++ "% within " ++ show (ceiling $ maximum $ 0 : take ((length xs * l) `div` 100) diff) | l <- levels]
            where diff = sort [abs $ ideal x - actual x | x <- xs]


generateHTML :: [(FilePath, [ProgressEntry])] -> IO LBS.ByteString
generateHTML xs = do
    htmlDir <- getDataFileName "html"
    report <- LBS.readFile $ htmlDir </> "progress.html"
    let f name | name == "progress-data.js" = return $ LBS.pack $ "var progress =\n" ++ generateJSON xs
               | name == "version.js" = return $ LBS.pack $ "var version = " ++ show (showVersion version)
               | otherwise = LBS.readFile $ htmlDir </> name
    runTemplate f report

generateJSON :: [(FilePath, [ProgressEntry])] -> String
generateJSON = concat . jsonList . map ((++"}") . unlines . f)
    where
        f (file,ps) =
            ("{\"name\":" ++ show (takeFileName file) ++ ", \"values\":") :
            indent (jsonList $ map g ps)

        shw = showDP 1
        g ProgressEntry{..} = jsonObject
            [("idealSecs",shw idealSecs),("idealPerc",shw idealPerc)
            ,("actualSecs",shw actualSecs),("actualPerc",shw actualPerc)]

indent = map ("  "++)
jsonList xs = zipWith (:) ('[':repeat ',') xs ++ ["]"]
jsonObject xs = "{" ++ intercalate ", " [show a ++ ":" ++ b | (a,b) <- xs] ++ "}"


{-# NOINLINE xterm #-}
xterm :: Bool
xterm = unsafePerformIO $
    -- Terminal.app uses "xterm-256color" as its env variable
    maybe False ("xterm" `isPrefixOf`) <$> lookupEnv "TERM"


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


-- | Call the program @shake-progress@ if it is on the @$PATH@. The program is called with
--   the following arguments:
--
-- * @--title=string@ - the string passed to @progressProgram@.
--
-- * @--state=Normal@, or one of @NoProgress@, @Normal@, or @Error@ to indicate
--   what state the progress bar should be in.
--
-- * @--value=25@ - the percent of the build that has completed, if not in @NoProgress@ state.
--
--   The program will not be called consecutively with the same @--state@ and @--value@ options.
--
--   Windows 7 or higher users can get taskbar progress notifications by placing the following
--   program in their @$PATH@: <https://github.com/ndmitchell/shake/releases>.
progressProgram :: IO (String -> IO ())
progressProgram = do
    exe <- findExecutable "shake-progress"
    case exe of
        Nothing -> return $ const $ return ()
        Just exe -> do
            ref <- newIORef Nothing
            return $ \msg -> do
                let failure = " Failure! " `isInfixOf` msg
                let perc = let (a,b) = break (== '%') msg
                           in if null b then "" else reverse $ takeWhile isDigit $ reverse a
                let key = (failure, perc)
                same <- atomicModifyIORef ref $ \old -> (Just key, old == Just key)
                let state | perc == "" = "NoProgress"
                          | failure = "Error"
                          | otherwise = "Normal"
                rawSystem exe $ ["--title=" ++ msg, "--state=" ++ state] ++ ["--value=" ++ perc | perc /= ""]
                return ()


-- | A simple method for displaying progress messages, suitable for using as 'Development.Shake.shakeProgress'.
--   This function writes the current progress to the titlebar every five seconds using 'progressTitlebar',
--   and calls any @shake-progress@ program on the @$PATH@ using 'progressProgram'.
progressSimple :: IO Progress -> IO ()
progressSimple p = do
    program <- progressProgram
    progressDisplay 5 (\s -> progressTitlebar s >> program s) p
