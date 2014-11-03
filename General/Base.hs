{-# LANGUAGE BangPatterns, CPP #-}

module General.Base(
    getProcessorCount,
    randomElem,
    fastNub, showQuote
    ) where

import Control.Concurrent
import Control.Exception.Extra
import Data.Char
import Data.List
import qualified Data.HashSet as Set
import System.Environment.Extra
import System.IO.Unsafe
import System.Random
import Development.Shake.Classes
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types
#endif


---------------------------------------------------------------------
-- Data.List

-- | Like 'nub', but the results may be in any order.
fastNub :: (Eq a, Hashable a) => [a] -> [a]
fastNub = f Set.empty
    where f seen [] = []
          f seen (x:xs) | x `Set.member` seen = f seen xs
                        | otherwise = x : f (Set.insert x seen) xs


showQuote :: String -> String
showQuote xs | any isSpace xs = "\"" ++ concatMap (\x -> if x == '\"' then "\"\"" else [x]) xs ++ "\""
             | otherwise = xs


---------------------------------------------------------------------
-- System.Info

#if __GLASGOW_HASKELL__ >= 704
-- Use the underlying GHC function
foreign import ccall getNumberOfProcessors :: IO CInt
#endif


{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
#if __GLASGOW_HASKELL__ >= 704
            if rtsSupportsBoundThreads then
                fmap fromIntegral $ getNumberOfProcessors
            else
#endif
                handle_ (const $ return 1) $ do
                    env <- lookupEnv "NUMBER_OF_PROCESSORS"
                    case env of
                        Just s | [(i,"")] <- reads s -> return i
                        _ -> do
                            src <- readFile "/proc/cpuinfo"
                            return $ length [() | x <- lines src, "processor" `isPrefixOf` x]


---------------------------------------------------------------------
-- System.Random

randomElem :: [a] -> IO a
randomElem xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i
