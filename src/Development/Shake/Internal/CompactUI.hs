
-- | Provide a Buck/Bazel style UI.
module Development.Shake.Internal.CompactUI(
    compactUI
    ) where

import Development.Shake.Internal.Options
import Development.Shake.Internal.Progress

import System.Time.Extra
import Control.Exception
import General.Thread
import Data.IORef
import Control.Monad


data S = S
    {sOutput :: [String] -- ^ Messages that haven't yet been printed, in reverse.
    ,sProgress :: String -- ^ Last progress message.
    ,sTraces :: [Maybe (String, String, Seconds)] -- ^ the traced items, in the order we display them
    ,sUnwind :: Int -- ^ Number of lines we used last time around
    }

emptyS = S [] "Starting..." [] 0

addOutput pri msg s = s{sOutput = msg : sOutput s}
addProgress x s = s{sProgress = x}

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


display :: Seconds -> S -> (S, String)
display time s = (s{sOutput=[], sUnwind=length post}, "\ESC[" ++ show (sUnwind s) ++ "A" ++ unlines (map pad $ pre ++ post))
    where
        pre = sOutput s
        post = "" : ("Progress: " ++ sProgress s) : map f (sTraces s)

        pad x = x ++ "\ESC[K"
        f Nothing = " *"
        f (Just (k,m,t)) = " * " ++ k ++ " (" ++ m ++ " " ++ showDuration (time - t) ++ ")"


-- | Run a compact UI, with the ShakeOptions modifier, combined with
compactUI :: ShakeOptions -> IO (ShakeOptions, IO ())
compactUI opts = do
    ref <- newIORef emptyS
    let tweak f = atomicModifyIORef ref $ \s -> (f s, ())
    time <- offsetTime
    opts <- return $ opts
        {shakeTrace = \a b c -> do t <- time;  tweak (addTrace a b c t)
        ,shakeOutput = \a b -> tweak (addOutput a b)
        ,shakeProgress = \x -> void $ progressDisplay 1 (tweak . addProgress) x `withThreadsBoth` shakeProgress opts x
        ,shakeVerbosity = Quiet
        }
    let tick = do t <- time; mask_ $ putStr =<< atomicModifyIORef ref (display t)
    return (opts, forever (tick >> sleep 0.2) `finally` tick)
