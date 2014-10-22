
module Main(main) where

import Model
import Control.Monad
import Test.QuickCheck

main :: IO ()
main = do
    forM_ props $ \(name,prop) -> do
        putStrLn $ "Testing " ++ name
        quickCheck prop
