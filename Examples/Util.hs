
module Examples.Util(module Examples.Util) where

import Development.Shake
import Development.Shake.FilePath

import Control.Concurrent
import Control.Monad
import Data.List
import System.Directory as IO
import System.Environment
import System.Random


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
        "test":extra -> do
            putStrLn $ "## TESTING " ++ name
            -- if the extra arguments are not --quiet/--loud it's probably going to go wrong
            test (\args -> withArgs (name:args ++ extra) $ shaken test rules) (out++)
            putStrLn $ "## FINISHED TESTING " ++ name
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

        "perturb":args -> forever $ do
            del <- removeFilesRandom out
            threads <- randomRIO (1,4)
            putStrLn $ "## TESTING PERTURBATION (" ++ show del ++ " files, " ++ show threads ++ " threads)"
            shake shakeOptions{shakeFiles=out, shakeThreads=threads, shakeVerbosity=Quiet} $ rules args (out++)

        args -> do
            (flags,args) <- return $ partition ("-" `isPrefixOf`) args
            let f o x = let x2 = dropWhile (== '-') x in case lookup x2 flagList of
                    Just op -> op o
                    Nothing | "threads" `isPrefixOf` x2 -> o{shakeThreads=read $ drop 7 x2}
                            | otherwise -> error $ "Don't know how to deal with flag: " ++ x
            let opts = foldl' f shakeOptions{shakeFiles=out, shakeReport=Just $ "output/" ++ name ++ "/report.html"} flags
            shake opts $ rules args (out++)


flags :: [String]
flags = "threads#" : map fst flagList


flagList :: [(String, ShakeOptions -> ShakeOptions)]
flagList = let (*) = (,) in
    ["no-dump" * \o -> o{shakeReport=Nothing}
    ,"silent" * \o -> o{shakeVerbosity=Silent}
    ,"quiet" * \o -> o{shakeVerbosity=Quiet}
    ,"normal" * \o -> o{shakeVerbosity=Normal}
    ,"loud" * \o -> o{shakeVerbosity=Loud}
    ,"diagnostic" * \o -> o{shakeVerbosity=Diagnostic}
    ,"staunch" * \o -> o{shakeStaunch=True}
    ,"deterministic" * \o -> o{shakeDeterministic=True}
    ,"lint" * \o -> o{shakeLint=True}
    ]


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1

assert :: Bool -> String -> IO ()
assert b msg = unless b $ error $ "ASSERTION FAILED: " ++ msg

infix 4 ===

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


removeFilesRandom :: FilePath -> IO Int
removeFilesRandom x = do
    files <- getDirectoryContentsRecursive x
    n <- randomRIO (0,length files)
    rs <- replicateM (length files) (randomIO :: IO Double)
    mapM_ (removeFile . snd) $ sort $ zip rs files
    return n


getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- IO.getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive dirs
    return $ files++rest
    where
        isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return $ if res then (x:as,bs) else (as,x:bs)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

