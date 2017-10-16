{-# LANGUAGE ScopedTypeVariables #-}

module General.Extra(
    getProcessorCount,
    withResultType,
    whenLeft,
    randomElem,
    wrapQuote, showBracket,
    withs,
    maximum', maximumBy',
    fastAt,
    forkFinallyUnmasked,
    isAsyncException,
    removeFile_,
    catchIO, tryIO,
    ) where

import Control.Exception
import Data.Char
import Data.List
import System.Environment.Extra
import System.IO.Extra
import System.IO.Unsafe
import System.Random
import System.Directory
import System.Exit
import Control.Concurrent
import Data.Functor
import Data.Primitive.Array
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import GHC.Conc(getNumProcessors)
import Prelude


---------------------------------------------------------------------
-- Prelude

-- See https://ghc.haskell.org/trac/ghc/ticket/10830 - they broke maximumBy
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' $ \x y -> if cmp x y == GT then x else y

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare


---------------------------------------------------------------------
-- Data.List

-- | If a string has any spaces then put quotes around and double up all internal quotes.
--   Roughly the inverse of Windows command line parsing.
wrapQuote :: String -> String
wrapQuote xs | any isSpace xs = "\"" ++ concatMap (\x -> if x == '\"' then "\"\"" else [x]) xs ++ "\""
             | otherwise = xs

-- | If a string has any spaces then put brackets around it.
wrapBracket :: String -> String
wrapBracket xs | any isSpace xs = "(" ++ xs ++ ")"
               | otherwise = xs

-- | Alias for @wrapBracket . show@.
showBracket :: Show a => a -> String
showBracket = wrapBracket . show


-- | Version of '!!' which is fast and returns 'Nothing' if the index is not present.
fastAt :: [a] -> (Int -> Maybe a)
fastAt xs = \i -> if i < 0 || i >= n then Nothing else Just $ indexArray arr i
    where
        n = length xs
        arr = runST $ do
            let n = length xs
            arr <- newArray n undefined
            forM_ (zip [0..] xs) $ \(i,x) ->
                writeArray arr i x
            unsafeFreezeArray arr


---------------------------------------------------------------------
-- System.Info

{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
            if rtsSupportsBoundThreads then
                fromIntegral <$> getNumProcessors
            else do
                env <- lookupEnv "NUMBER_OF_PROCESSORS"
                case env of
                    Just s | [(i,"")] <- reads s -> return i
                    _ -> do
                        src <- readFile' "/proc/cpuinfo" `catchIO` \_ -> return ""
                        return $! max 1 $ length [() | x <- lines src, "processor" `isPrefixOf` x]


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


---------------------------------------------------------------------
-- Control.Concurrent

-- | Like 'forkFinally', but the inner thread is unmasked even if you started masked.
forkFinallyUnmasked :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinallyUnmasked act cleanup =
    mask_ $ forkIOWithUnmask $ \unmask ->
        try (unmask act) >>= cleanup


---------------------------------------------------------------------
-- Control.Exception

-- | Is the exception asynchronous, not a "coding error" that should be ignored
isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = Control.Exception.catch -- GHC 7.4 has catch in the Prelude as well

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


---------------------------------------------------------------------
-- System.Directory

-- | Remove a file, but don't worry if it fails
removeFile_ :: FilePath -> IO ()
removeFile_ x = removeFile x `catchIO` \_ -> return ()


---------------------------------------------------------------------
-- Data.Either

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = either f (const $ pure ()) x


---------------------------------------------------------------------
-- Data.Proxy

-- Should be Proxy, but that's not available in older GHC 7.6 and before
withResultType :: (Maybe a -> a) -> a
withResultType f = f Nothing
