
module Examples.Util(module Examples.Util) where

import Development.Shake
import Development.Shake.FilePath

import Control.Concurrent
import Control.Monad
import Data.List
import System.Directory as IO
import System.Environment


shaken
    :: (([String] -> IO ()) -> (String -> String) -> IO ())
    -> ([String] -> (String -> String) -> Rules ())
    -> IO ()
shaken test rules = do
    name:args <- getArgs
    putStrLn $ "## BUILD " ++ unwords (name:args)
    let out = "output/" ++ name ++ "/"
    createDirectoryIfMissing True out
    case args of
        "test":_ -> do
            putStrLn $ "## TESTING " ++ name
            test (\args -> withArgs (name:args) $ shaken test rules) (out++)
        "clean":_ -> removeDirectoryRecursive out
{-
        "lint":args -> do
            let dbfile = out ++ ".database"
                tempfile = "output/" ++ name ++ ".database"
            b <- IO.doesFileExist dbfile
            when b $ renameFile dbfile tempfile
            removeDirectoryRecursive out
            createDirectoryIfMissing True out
            when b $ renameFile tempfile dbfile
            shake shakeOptions{shakeFiles=out, shakeLint=True} $ rules args (out++)
-}
        args -> do
            (flags,args) <- return $ partition ("-" `isPrefixOf`) args
            let f o x = let x2 = dropWhile (== '-') x in case lookup x2 flagList of
                    Just op -> op o
                    Nothing | "threads" `isPrefixOf` x2 -> o{shakeParallel=read $ drop 7 x2}
                            | otherwise -> error $ "Don't know how to deal with flag: " ++ x
            let opts = foldl' f shakeOptions{shakeFiles=out, shakeDump=True} $ flags
            shake opts $ rules args (out++)


flags :: [String]
flags = "threads#" : map fst flagList


flagList :: [(String, ShakeOptions -> ShakeOptions)]
flagList = let (*) = (,) in
    ["no-dump" * \o -> o{shakeDump=False}
    ,"silent" * \o -> o{shakeVerbosity=Silent}
    ,"quiet" * \o -> o{shakeVerbosity=Quiet}
    ,"normal" * \o -> o{shakeVerbosity=Normal}
    ,"loud" * \o -> o{shakeVerbosity=Loud}
    ,"diagnostic" * \o -> o{shakeVerbosity=Diagnostic}
    ]


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1

assert :: Bool -> String -> IO ()
assert b msg = unless b $ error $ "ASSERTION FAILED: " ++ msg


(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assert (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b


assertContents :: FilePath -> String -> IO ()
assertContents file want = do
    got <- readFile file
    assert (want == got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got


noTest :: ([String] -> IO ()) -> (String -> String) -> IO ()
noTest build obj = do
    build []
    build []


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


-- | Sleep long enough for the modification time resolution to catch up
sleepFileTime :: IO ()
sleepFileTime = sleep 1

