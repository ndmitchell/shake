
module General.Extra(
    getProcessorCount,
    randomElem,
    showQuote,
    withs,
    maximum', maximumBy'
    ) where

import Control.Exception.Extra
import Data.Char
import Data.List
import System.Environment.Extra
import System.IO.Extra
import System.IO.Unsafe
import System.Random
import Control.Concurrent
import Foreign.C.Types


---------------------------------------------------------------------
-- Data.List

showQuote :: String -> String
showQuote xs | any isSpace xs = "\"" ++ concatMap (\x -> if x == '\"' then "\"\"" else [x]) xs ++ "\""
             | otherwise = xs


---------------------------------------------------------------------
-- System.Info

-- Use the underlying GHC function
foreign import ccall getNumberOfProcessors :: IO CInt


{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
            if rtsSupportsBoundThreads then
                fmap fromIntegral $ getNumberOfProcessors
            else
                handle_ (const $ return 1) $ do
                    env <- lookupEnv "NUMBER_OF_PROCESSORS"
                    case env of
                        Just s | [(i,"")] <- reads s -> return i
                        _ -> do
                            src <- readFile' "/proc/cpuinfo"
                            return $! length [() | x <- lines src, "processor" `isPrefixOf` x]


---------------------------------------------------------------------
-- System.Random

randomElem :: [a] -> IO a
randomElem xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i


---------------------------------------------------------------------
-- Control.Monad

withs :: [(a -> r) -> r] -> ([a] -> r) -> r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as


-- See https://ghc.haskell.org/trac/ghc/ticket/10830 - they broke maximumBy
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' $ \x y -> if cmp x y == GT then x else y

maximum' :: Ord a => [a] -> a
maximum' xs = maximumBy' compare xs
