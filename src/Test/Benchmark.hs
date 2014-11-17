
module Test.Benchmark(main) where

import Development.Shake
import Test.Type
import Data.List
import Development.Shake.FilePath


-- | Given a breadth and depth come up with a set of build files
main = shaken (\_ _ -> return ()) $ \args obj -> do
    let get ty = head $ [read $ drop (length ty + 1) a | a <- args, (ty ++ "=") `isPrefixOf` a] ++
                        error ("Could not find argument, expected " ++ ty ++ "=Number")
        depth = get "depth"
        breadth = get "breadth"

    want [obj $ "0." ++ show i | i <- [1..breadth]]
    obj "*" %> \out -> do
        let d = read $ takeBaseName out
        need [obj $ show (d + 1) ++ "." ++ show i | d < depth, i <- [1..breadth]]
        writeFile' out ""
