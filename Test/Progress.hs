{-# LANGUAGE DeriveDataTypeable #-}

module Test.Progress(main) where

import Development.Shake.Progress
import Development.Shake.Classes
import Test.Type
import qualified Control.Exception as E
import Data.IORef
import Data.Monoid
import Data.Char


main = shaken test $ \args obj -> return ()


-- | Given a list of todo times, get out a list of how long is predicted
prog = progEx 10000000000000000

data MyException = MyException deriving (Show, Typeable)
instance E.Exception MyException


progEx :: Double -> [Double] -> IO [Double]
progEx mxDone todo = do
    let resolution = 10000 -- Use resolution to get extra detail on the numbers
    let done = scanl (+) 0 $ map (min mxDone . max 0) $ zipWith (-) todo (tail todo)
    pile <- newIORef $ tail $ zipWith (\t d -> mempty{timeBuilt=d*resolution,timeTodo=(t*resolution,0)}) todo done
    let get = do a <- readIORef pile
                 case a of
                     [] -> E.throw MyException
                     x:xs -> do writeIORef pile xs; return x

    out <- newIORef []
    let put x = do let (mins,secs) = break (== 'm') $ takeWhile (/= '(') x
                   let f x = let y = filter isDigit x in if null y then 0/0 else read y
                   modifyIORef out (++ [(if null secs then f mins else f mins * 60 + f secs) / resolution])
    -- we abort by killing the thread, but then catch the abort and resume normally
    E.catch (progressDisplayTester resolution put get) $ \MyException -> return ()
    fmap (take $ length todo) $ readIORef out


test build obj = do
    -- perfect functions should match perfectly
    xs <- prog [10,9..1]
    drop 2 xs === [8,7..1]
    xs <- prog $ map (*5) [10,9..1]
    drop 2 xs === [8,7..1]
    xs <- prog $ map (*0.2) [10,9..1]
    let dp3 x = fromIntegral (round $ x * 1000 :: Int) / 1000
    map dp3 (drop 2 xs) === [8,7..1]

    -- The properties below this line could be weakened

    -- increasing functions can't match
    xs <- prog [5,6,7]
    last xs === 7

    -- the first value must be plausible, or missing
    xs <- prog [187]
    assert (isNaN $ head xs) "No first value"

    -- desirable properties, could be weakened
    xs <- progEx 2 $ 100:map (*2) [10,9..1]
    drop 5 xs === [6,5..1]
    xs <- progEx 1 $ [10,9,100,8,7,6,5,4,3,2,1]
    assert (all (<= 1.5) $ map abs $ zipWith (-) (drop 5 xs) [6,5..1]) "Close"

    -- if no progress is made, don't keep the time going up
    xs <- prog [10,9,8,7,7,7,7,7]
    drop 5 xs === [7,7,7]

    -- if the work rate changes, should somewhat reflect that
    xs <- prog [10,9,8,7,6.5,6,5.5,5]
    assert (last xs > 7.1) "Some discounting (factor=0 would give 7)"
