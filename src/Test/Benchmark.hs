
module Test.Benchmark(main) where

import General.GetOpt
import Development.Shake
import Test.Type
import Development.Shake.FilePath


data Opts = Depth Int | Breadth Int
opts = [Option "" ["depth"  ] (ReqArg (fmap Depth   . readEither) "INT") ""
       ,Option "" ["breadth"] (ReqArg (fmap Breadth . readEither) "INT") ""]

readEither = Right . read -- FIXME: Use the version from extra once available

-- | Given a breadth and depth come up with a set of build files
main = shakeTest test opts $ \opts -> do
    let depth   = last $ error "Missing --depth"   : [x | Depth   x <- opts]
    let breadth = last $ error "Missing --breadth" : [x | Breadth x <- opts]

    want ["0." ++ show i | i <- [1..breadth]]
    "*" %> \out -> do
        let d = read $ takeBaseName out
        need [show (d + 1) ++ "." ++ show i | d < depth, i <- [1..breadth]]
        writeFile' out ""

test build = do
    -- these help to test the stack limit
    build ["clean"]
    build ["--breadth=75","--depth=75"]
    build ["--breadth=75","--depth=75"]
