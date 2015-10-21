
module Test.Type(sleep, module Test.Type) where

import Development.Shake hiding (copyFileChanged)
import Development.Shake.Rule() -- ensure the module gets imported, and thus tested
import General.String
import General.Extra
import Development.Shake.FileInfo
import Development.Shake.FilePath

import Control.Exception.Extra hiding (assert)
import Control.Monad.Extra
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import System.Directory as IO
import System.Environment.Extra
import System.Random
import System.Console.GetOpt
import System.IO.Extra as IO
import System.Time.Extra
import Prelude


shaken
    :: (([String] -> IO ()) -> (String -> String) -> IO ())
    -> ([String] -> (String -> String) -> Rules ())
    -> IO ()
    -> IO ()
shaken test rules sleeper = do
    name:args <- getArgs
    when ("--sleep" `elem` args) sleeper
    putStrLn $ "## BUILD " ++ unwords (name:args)
    args <- return $ delete "--sleep" args
    let out = "output/" ++ name ++ "/"
    let obj x = if "/" `isPrefixOf` x then init out ++ x else out ++ x
    createDirectoryIfMissing True out
    case args of
        "test":extra -> do
            putStrLn $ "## TESTING " ++ name
            -- if the extra arguments are not --quiet/--loud it's probably going to go wrong
            test (\args -> withArgs (name:args ++ extra) $ shaken test rules sleeper) obj
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
            cwd <- getCurrentDirectory
            t <- tracker
            let (_,files,_) = getOpt Permute [] args

            withArgs (args \\ files) $
                shakeWithClean
                    (removeDirectoryRecursive out)
                    (shakeOptions{shakeFiles = out
                                 ,shakeReport = ["output/" ++ name ++ "/report.html"]
                                 ,shakeLint = Just t
                                 ,shakeLintInside = [cwd]
                                 ,shakeLintIgnore = map (cwd </>) [".cabal-sandbox//",".stack-work//"]
                                 })
                    -- if you have passed sleep, supress the "no errors" warning
                    (do rules files obj; when ("--sleep" `elem` args) $ action $ return ())


shaken2
    :: (([String] -> IO ()) -> (String -> String) -> IO ())
    -> ([String] -> (String -> String) -> Rules ())
    -> IO ()
    -> IO ()
shaken2 test rules sleeper = shaken test rules2 sleeper
    where
        rules2 args obj = do
            (objd,args) <- return $ partition ("$" `isPrefixOf`) args
            (spec,phon) <- return $ partition ("!" `isPrefixOf`) args
            want $ phon ++ map (obj . tail) objd
            rules (map tail spec) obj


tracker :: IO Lint
tracker = do
  fsatrace <- lookupEnv "FSAT"
  trackerExe <- findExecutable "tracker.exe"
  return $ if False && isJust fsatrace
           then LintFSATrace
           else if False && isJust trackerExe
                then LintTracker
                else LintBasic

hasTracker :: IO Bool
hasTracker = do
  t <- tracker
  return $ t == LintFSATrace || t == LintTracker


shakeWithClean :: IO () -> ShakeOptions -> Rules () -> IO ()
shakeWithClean clean opts rules = shakeArgsWith opts [cleanOpt] f
    where
        cleanOpt = Option "c" ["clean"] (NoArg $ Right ()) "Clean before building."

        f extra files = do
            when (extra /= []) clean
            if "clean" `elem` files then
                clean >> return Nothing
             else
                return $ Just $ if null files then rules else want files >> withoutActions rules


unobj :: FilePath -> FilePath
unobj = dropDirectory1 . dropDirectory1

assert :: Bool -> String -> IO ()
assert b msg = unless b $ error $ "ASSERTION FAILED: " ++ msg

infix 4 ===

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assert (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b


assertExists :: FilePath -> IO ()
assertExists file = do
    b <- IO.doesFileExist file
    assert b $ "File was expected to exist, but is missing: " ++ file

assertMissing :: FilePath -> IO ()
assertMissing file = do
    b <- IO.doesFileExist file
    assert (not b) $ "File was expected to be missing, but exists: " ++ file

assertContents :: FilePath -> String -> IO ()
assertContents file want = do
    got <- IO.readFile' file
    assert (want == got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got

assertContentsOn :: (String -> String) -> FilePath -> String -> IO ()
assertContentsOn f file want = do
    got <- IO.readFile' file
    assert (f want == f got) $ "File contents are wrong: " ++ file ++ "\nWANT: " ++ want ++ "\nGOT: " ++ got ++
                               "\nWANT (transformed): " ++ f want ++ "\nGOT (transformed): " ++ f got

assertContentsWords :: FilePath -> String -> IO ()
assertContentsWords = assertContentsOn (unwords . words)


assertContentsInfix :: FilePath -> String -> IO ()
assertContentsInfix file want = do
    got <- IO.readFile' file
    assert (want `isInfixOf` got) $ "File contents are wrong: " ++ file ++ "\nWANT (anywhere): " ++ want ++ "\nGOT: " ++ got

assertContentsUnordered :: FilePath -> [String] -> IO ()
assertContentsUnordered file xs = assertContentsOn (unlines . sort . lines) file (unlines xs)

assertException :: [String] -> IO () -> IO ()
assertException parts act = do
    res <- try_ act
    case res of
        Left err -> let s = show err in forM_ parts $ \p ->
            assert (p `isInfixOf` s) $ "Incorrect exception, missing part:\nGOT: " ++ s ++ "\nWANTED: " ++ p
        Right _ -> error "Expected an exception but succeeded"


noTest :: ([String] -> IO ()) -> (String -> String) -> IO ()
noTest build obj = do
    build ["--abbrev=output=$OUT","-j3"]
    build ["--no-build","--report=-"]
    build []


-- | Sleep long enough for the modification time resolution to catch up
sleepFileTime :: IO ()
sleepFileTime = sleep 1


sleepFileTimeCalibrate :: IO (IO ())
sleepFileTimeCalibrate = do
    let file = "output/calibrate"
    createDirectoryIfMissing True $ takeDirectory file
    mtimes <- forM [1..10] $ \i -> fmap fst $ duration $ do
        writeFile file $ show i
        let time = fmap (fst . fromMaybe (error "File missing during sleepFileTimeCalibrate")) $ getFileInfo $ packU file
        t1 <- time
        flip loopM 0 $ \j -> do
            writeFile file $ show (i,j)
            t2 <- time
            return $ if t1 == t2 then Left $ j+1 else Right ()
    putStrLn $ "Longest file modification time lag was " ++ show (ceiling (maximum' mtimes * 1000)) ++ "ms"
    return $ sleep $ min 1 $ maximum' mtimes * 2


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
    (dirs,files) <- partitionM IO.doesDirectoryExist [dir </> x | x <- xs, not $ "." `isPrefixOf` x]
    rest <- concatMapM getDirectoryContentsRecursive dirs
    return $ files++rest


copyDirectoryChanged :: FilePath -> FilePath -> IO ()
copyDirectoryChanged old new = do
    xs <- getDirectoryContentsRecursive old
    forM_ xs $ \from -> do
        let to = new </> drop (length $ addTrailingPathSeparator old) from
        createDirectoryIfMissing True $ takeDirectory to
        copyFileChanged from to


copyFileChanged :: FilePath -> FilePath -> IO ()
copyFileChanged old new = do
    good <- IO.doesFileExist new
    good <- if not good then return False else liftM2 (==) (BS.readFile old) (BS.readFile new)
    when (not good) $ copyFile old new
