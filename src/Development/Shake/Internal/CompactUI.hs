
-- | Provide a Buck/Bazel style UI.
module Development.Shake.Internal.CompactUI(
    compactUI
    ) where

import Development.Shake.Internal.CmdOption
import Development.Shake.Internal.Options
import Development.Shake.Internal.Progress
import Development.Shake.Internal.TermSize

import System.Time.Extra
import General.Extra
import Control.Exception
import General.Thread
import General.EscCodes
import Data.IORef.Extra
import Control.Monad.Extra
import Data.Maybe


data S = S
    {sOutput :: [String] -- ^ Messages that haven't yet been printed, in reverse.
    ,sProgress :: String -- ^ Last progress message.
    ,sTraces :: [Maybe (String, String, Seconds)] -- ^ the traced items, in the order we display them
    ,sUnwind :: Int -- ^ Number of lines we used last time around
    }

startString = escForeground Green ++ escBold ++ "Starting" ++ escNormal ++ "..."
emptyS = S [] "" [] 0

progressToString Starting      = startString
progressToString (Finished t)  = "Finished in " ++ showDuration t
progressToString (Executing p t secs perc done todo predicted) =
    let failed = maybe "" (", Failure! " ++) (isFailure p)
        sdone  = escBold ++ escForeground Blue ++ show done ++ escNormal ++ escBold
        stodo  = escForeground Green ++ show todo ++ escNormal
        spred | floor perc < 20 = escBold ++ escForeground Red    ++ predicted ++ escNormal
              | floor perc < 60 = escBold ++ escForeground Yellow ++ predicted ++ escNormal
              | otherwise       = escBold ++ escForeground Green  ++ predicted ++ escNormal
    in "Building for " ++ showDurationSecs t ++ " [" ++ sdone ++ "/" ++ stodo ++ "]" ++
       ", ETA: " ++ spred ++ failed

addOutput pri msg s = s{sOutput = msg : sOutput s}
addProgress x s = s{sProgress = progressToString x}

addTrace key msg start time s
    | start = s{sTraces = insert (key,msg,time) $ sTraces s}
    | otherwise = s{sTraces = remove (\(a,b,_) -> a == key && b == msg) $ sTraces s}
    where
        insert v (Nothing:xs) = Just v:xs
        insert v (x:xs) = x : insert v xs
        insert v [] = [Just v]

        remove f (Just x:xs) | f x = Nothing:xs
        remove f (x:xs) = x : remove f xs
        remove f [] = []

clearCursorUp n = concat (replicate n (escClearLine ++ escCursorUp 1))

display :: Bool -> Int -> Seconds -> S -> (S, String)
display True _ _ s = (s, clearCursorUp (sUnwind s) ++ escClearLine)
display False cols time s = (s{sOutput=[], sUnwind=length post}, clearCursorUp (sUnwind s) ++ unlines (map pad $ pre ++ post))
    where
        pre = sOutput s
        post = (sProgress s ++ escNormal) : mapMaybe f (sTraces s)

        pad x = x ++ escClearLine
        f Nothing = Nothing
        f (Just (k,m,t)) = Just result
          where
            full = " * " ++ k ++ " (" ++ g (time - t) m ++ ")"
            full_size = length full

            elide_size = (cols - 3) `div` 2 -- space for '...'
            start = take elide_size full
            end = drop (full_size - elide_size) full

            result | full_size > cols = start ++ "..." ++ end
                   | otherwise        = full

        g i m = case i of
          -- fast things just show the command
          _ | dur == "0s" -> cmd
          -- fast-ish things show command + time taken
          _ | i < 10 -> cmd ++ " " ++ dur
          -- slow commands show colored results
          _ | otherwise -> alert ++ cmd ++ " " ++ alert ++ dur ++ escNormal
          where dur   = showDurationSecs i
                cmd   = escBold ++ m ++ escNormal
                alert = escForeground (if i > 20 then Red else Yellow)

-- | Run a compact UI, with the ShakeOptions modifier, combined with
compactUI :: ShakeOptions -> IO (ShakeOptions, IO ())
compactUI opts = do
    unlessM checkEscCodes $
        putStrLn "Your terminal does not appear to support escape codes, --compact mode may not work"
    ref <- newIORef emptyS
    let tweak = atomicModifyIORef_ ref
    time <- offsetTime
    (_rows, columns) <- getTermSize
    opts <- return $ opts
        {shakeTrace = \a b c -> do t <- time; tweak (addTrace a b c t)
        ,shakeOutput = \a b -> tweak (addOutput a b)
        ,shakeProgress = \x -> void $ withThreadsBoth
            (progressRaw 1 (tweak . addProgress) x)
            (shakeProgress opts x)
        ,shakeCommandOptions = [EchoStdout False, EchoStderr False] ++ shakeCommandOptions opts
        ,shakeVerbosity = Error
        }

    let tick final = do t <- time; mask_ $ putStr =<< atomicModifyIORef ref (display final columns t)
    return (opts, forever (tick False >> sleep 0.4) `finally` tick True)
