{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RecordWildCards, GeneralizedNewtypeDeriving, ViewPatterns, Rank2Types #-}

module General.Extra(
    getProcessorCount,
    findGcc,
    whenLeft,
    randomElem,
    wrapQuote, showBracket,
    withs, forNothingM,
    maximum', maximumBy',
    fastAt,
    zipExact, zipWithExact,
    isAsyncException,
    showDurationSecs,
    usingLineBuffering,
    doesFileExist_,
    usingNumCapabilities,
    removeFile_, createDirectoryRecursive,
    catchIO, tryIO, handleIO, handleSynchronous,
    Located, Partial, callStackTop, callStackFull, withFrozenCallStack, callStackFromException,
    Ver(..), makeVer,
    QTypeRep(..),
    NoShow(..)
    ) where

import Control.Exception.Extra
import Data.Char
import Data.List.Extra
import System.Environment
import Development.Shake.FilePath
import Control.DeepSeq
import Numeric
import General.Cleanup
import Data.Typeable
import System.IO.Extra
import System.Time.Extra
import System.IO.Unsafe
import System.Info.Extra
import System.Random
import System.Directory
import System.Exit
import Numeric.Extra
import Foreign.Storable
import Control.Concurrent.Extra
import Data.Maybe
import Data.Hashable
import Data.Primitive.Array
import Control.Monad
import Control.Monad.ST
import GHC.Conc(getNumProcessors)
#if __GLASGOW_HASKELL__ >= 800
import GHC.Stack
#endif


---------------------------------------------------------------------
-- Prelude

-- See https://ghc.haskell.org/trac/ghc/ticket/10830 - they broke maximumBy
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' $ \x y -> if cmp x y == GT then x else y

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare

newtype NoShow a = NoShow a
instance Show (NoShow a) where show _ = "NoShow"


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
            forM_ (zipFrom 0 xs) $ \(i,x) ->
                writeArray arr i x
            unsafeFreezeArray arr

zipWithExact :: Partial => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExact f = g
    where
        g [] [] = []
        g (a:as) (b:bs) = f a b : g as bs
        g _ _ = error "zipWithExacts: unequal lengths"

zipExact :: Partial => [a] -> [b] -> [(a,b)]
zipExact = zipWithExact (,)


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


-- Can you find a GCC executable? return a Bool, and optionally something to add to $PATH to run it
findGcc :: IO (Bool, Maybe FilePath)
findGcc = do
    v <- findExecutable "gcc"
    case v of
        Nothing | isWindows -> do
            ghc <- findExecutable "ghc"
            case ghc of
                Just ghc -> do
                    let gcc = takeDirectory (takeDirectory ghc) </> "mingw/bin/gcc.exe"
                    b <- doesFileExist_ gcc
                    return $ if b then (True, Just $ takeDirectory gcc) else (False, Nothing)
                _ -> return (False, Nothing)
        _ -> return (isJust v, Nothing)



---------------------------------------------------------------------
-- System.Random

randomElem :: [a] -> IO a
randomElem xs = do
    when (null xs) $ fail "General.Extra.randomElem called with empty list, can't pick a random element"
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i


---------------------------------------------------------------------
-- System.IO

usingLineBuffering :: Cleanup -> IO ()
usingLineBuffering cleanup = do
    out <- hGetBuffering stdout
    err <- hGetBuffering stderr
    when (out /= LineBuffering || err /= LineBuffering) $ do
        register cleanup $ hSetBuffering stdout out >> hSetBuffering stderr err
        hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering


---------------------------------------------------------------------
-- System.Time

showDurationSecs :: Seconds -> String
showDurationSecs = replace ".00s" "s" . showDuration . intToDouble . round


---------------------------------------------------------------------
-- Control.Monad

withs :: [(a -> r) -> r] -> ([a] -> r) -> r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as

forNothingM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe [b])
forNothingM [] f = return $ Just []
forNothingM (x:xs) f = do
    v <- f x
    case v of
        Nothing -> return Nothing
        Just v -> liftM (v:) `liftM` forNothingM xs f


---------------------------------------------------------------------
-- Control.Concurrent

usingNumCapabilities :: Cleanup -> Int -> IO ()
usingNumCapabilities cleanup new = when rtsSupportsBoundThreads $ do
    old <- getNumCapabilities
    when (old /= new) $ do
        register cleanup $ setNumCapabilities old
        setNumCapabilities new


---------------------------------------------------------------------
-- Control.Exception

-- | Is the exception asynchronous, not a "coding error" that should be ignored
isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = handle

handleSynchronous :: (SomeException -> IO a) -> IO a -> IO a
handleSynchronous = handleJust (\e -> guard (not $ isAsyncException e) >> pure e)

---------------------------------------------------------------------
-- System.Directory

doesFileExist_ :: FilePath -> IO Bool
doesFileExist_ x = doesFileExist x `catchIO` \_ -> return False

-- | Remove a file, but don't worry if it fails
removeFile_ :: FilePath -> IO ()
removeFile_ x = removeFile x `catchIO` \_ -> return ()

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
    x <- tryIO $ doesDirectoryExist dir
    when (x /= Right True) $ createDirectoryIfMissing True dir


---------------------------------------------------------------------
-- Data.Either

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = either f (const $ pure ()) x


---------------------------------------------------------------------
-- Data.CallStack

type Located = Partial

callStackTop :: Partial => String
callStackTop = withFrozenCallStack $ head $ callStackFull ++ ["unknown location"]

callStackFull :: Partial => [String]
callStackFromException :: SomeException -> ([String], SomeException)


#if __GLASGOW_HASKELL__ >= 800

-- | Invert 'prettyCallStack', which GHC pre-applies in certain cases
parseCallStack = reverse . map trimStart . drop 1 . lines

callStackFull = parseCallStack $ prettyCallStack $ popCallStack callStack

callStackFromException (fromException -> Just (ErrorCallWithLocation msg loc)) = (parseCallStack loc, toException $ ErrorCall msg)
callStackFromException e = ([], e)

#else

callStackFull = []
callStackFromException e = ([], e)

withFrozenCallStack :: a -> a
withFrozenCallStack = id
#endif

---------------------------------------------------------------------
-- Data.Version

-- | A version number that indicates change, not ordering or compatibilty.
--   Always presented as an 'Int' to the user, but a newtype inside the library for safety.
newtype Ver = Ver Int
    deriving (Show,Eq,Storable)

makeVer :: String -> Ver
makeVer = Ver . hash


---------------------------------------------------------------------
-- Data.Typeable

-- | Like TypeRep, but the Show includes enough information to be unique
--   so I can rely on @a == b === show a == show b@.
newtype QTypeRep = QTypeRep {fromQTypeRep :: TypeRep}
    deriving (Eq,Hashable,NFData)

instance Show QTypeRep where
    show (QTypeRep x) = show x ++ " {" ++ showHex (abs $ hashWithSalt 0 x) "" ++ "}"
