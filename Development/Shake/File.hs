{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.File(
    need, want,
    defaultRuleFile,
    (*>), (**>), (?>), phony, (~>),
    newCache, newCacheIO
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import System.Directory

import Development.Shake.Core
import Development.Shake.Util
import Development.Shake.Classes
import Development.Shake.FilePattern
import Development.Shake.FileTime

import System.FilePath(takeDirectory) -- important that this is the system local filepath, or wrong slashes go wrong


infix 1 *>, ?>, **>, ~>


newtype FileQ = FileQ BSU
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show FileQ where show (FileQ x) = unpackU x

newtype FileA = FileA FileTime
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show FileA where show (FileA x) = "FileTime " ++ show x

instance Rule FileQ FileA where
    storedValue (FileQ x) = fmap (fmap FileA) $ getModTimeMaybe x

{-
    observed act = do
        src <- getCurrentDirectory
        old <- listDir src
        sleepFileTime
        res <- act
        new <- listDir src
        let obs = compareItems old new
            -- if we didn't find anything used, then most likely we aren't tracking access time close enough
            obs2 = obs{used = if used obs == Just [] then Nothing else (used obs)}
        return (obs2, res)


data Item = ItemDir [(String,Item)] -- sorted
          | ItemFile (Maybe FileTime) (Maybe FileTime) -- mod time, access time
            deriving Show

listDir :: FilePath -> IO Item
listDir root = do
    xs <- getDirectoryContents root
    xs <- return $ sort $ filter (not . all (== '.')) xs
    fmap ItemDir $ forM xs $ \x -> fmap ((,) x) $ do
        let s = root </> x
        b <- doesFileExist s
        if b then listFile s else listDir s

listFile :: FilePath -> IO Item
listFile x = do
    let f x = Control.Exception.catch (fmap Just x) $ \(_ :: SomeException) -> return Nothing
    mod <- f $ getModTime x
    acc <- f $ getAccTime x
    return $ ItemFile mod acc

compareItems :: Item -> Item -> Observed File
compareItems = f ""
    where
        f path (ItemFile mod1 acc1) (ItemFile mod2 acc2) =
            Observed (Just [File path | mod1 /= mod2]) (Just [File path | acc1 /= acc2])
        f path (ItemDir xs) (ItemDir ys) = mconcat $ map g $ zips xs ys
            where g (name, Just x, Just y) = f (path </> name) x y
                  g (name, x, y) = Observed (Just $ concatMap (files path) $ catMaybes [x,y]) Nothing
        f path _ _ = Observed (Just [File path]) Nothing

        files path (ItemDir xs) = concat [files (path </> a) b | (a,b) <- xs]
        files path _ = [File path]

        zips :: Ord a => [(a,b)] -> [(a,b)] -> [(a, Maybe b, Maybe b)]
        zips ((x1,x2):xs) ((y1,y2):ys)
            | x1 == y1  = (x1,Just x2,Just y2):zips xs ys
            | x1 <  y1  = (x1,Just x2,Nothing):zips xs ((y1,y2):ys)
            | otherwise = (y1,Nothing,Just y2):zips ((x1,x2):xs) ys
        zips xs ys = [(a,Just b,Nothing) | (a,b) <- xs] ++ [(a,Nothing,Just b) | (a,b) <- ys]
-}


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleFile :: Rules ()
defaultRuleFile = defaultRule $ \(FileQ x) -> Just $
    liftIO $ fmap FileA $ getModTimeError "Error, file does not exist and no rule available:" x


-- | Require that the following files are built before continuing. Particularly
--   necessary when calling 'Development.Shake.cmd' or 'Development.Shake.command'. As an example:
--
-- @
-- \"\/\/*.rot13\" '*>' \\out -> do
--     let src = 'Development.Shake.FilePath.dropExtension' out
--     'need' [src]
--     'Development.Shake.cmd' \"rot13\" [src] \"-o\" [out]
-- @
need :: [FilePath] -> Action ()
need xs = (apply $ map (FileQ . packU) xs :: Action [FileA]) >> return ()

-- | Require that the following are built by the rules, used to specify the target.
--
-- @
-- main = 'Development.Shake.shake' 'shakeOptions' $ do
--    'want' [\"Main.exe\"]
--    ...
-- @
--
--   This program will build @Main.exe@, given sufficient rules.
--
--   This function is defined in terms of 'action' and 'need', use 'action' if you need more complex
--   targets than 'want' allows.
want :: [FilePath] -> Rules ()
want = action . need


root :: String -> (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
root help test act = rule $ \(FileQ x_) -> let x = unpackU x_ in
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        act x
        liftIO $ fmap FileA $ getModTimeError ("Error, rule " ++ help ++ " failed to build file:") x_


-- | Declare a phony action, this is an action that does not produce a file, and will be rerun
--   in every execution that requires it. You can demand 'phony' rules using 'want' \/ 'need'.
--
--   Phony actions are intended to define command-line abbreviations. You should not 'need' phony actions
--   as dependencies of rules, as that will cause excessive rebuilding.
phony :: String -> Action () -> Rules ()
phony name act = rule $ \(FileQ x_) -> let x = unpackU x_ in
    if name /= x then Nothing else Just $ do
        act
        return $ FileA fileTimeNone

-- | Infix operator alias for 'phony', for sake of consistency with normal
--   rules.
(~>) :: String -> Action () -> Rules ()
(~>) = phony 


-- | Define a rule to build files. If the first argument returns 'True' for a given file,
--   the second argument will be used to build it. Usually '*>' is sufficient, but '?>' gives
--   additional power. For any file used by the build system, only one rule should return 'True'.
--
-- @
-- (all isUpper . 'Development.Shake.FilePath.takeBaseName') '?>' \\out -> do
--     let src = 'Development.Shake.FilePath.replaceBaseName' out $ map toLower $ takeBaseName out
--     'Development.Shake.writeFile'' out . map toUpper =<< 'Development.Shake.readFile'' src
-- @
(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) = root "with ?>"


-- | Define a set of patterns, and if any of them match, run the associated rule. See '*>'.
(**>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(**>) test = root "with **>" (\x -> any (?== x) test)

-- | Define a rule that matches a 'FilePattern'. No file required by the system must be
--   matched by more than one pattern. For the pattern rules, see '?=='.
--
-- @
-- \"*.asm.o\" '*>' \\out -> do
--     let src = 'Development.Shake.FilePath.dropExtension' out
--     'need' [src]
--     'Development.Shake.cmd' \"as\" [src] \"-o\" [out]
-- @
--
--   To define a build system for multiple compiled languages, we recommend using @.asm.o@,
--   @.cpp.o@, @.hs.o@, to indicate which language produces an object file.
--   I.e., the file @foo.cpp@ produces object file @foo.cpp.o@.
--
--   Note that matching is case-sensitive, even on Windows.
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(*>) test = root (show test) (test ?==)


-- | A version of 'newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'newCache' instead.
newCacheIO :: (FilePath -> IO a) -> IO (FilePath -> Action a)
newCacheIO act = do
    var <- newVar Map.empty -- Var (Map FilePath (Barrier (Either SomeException a)))
    let run = either (\e -> throwIO (e :: SomeException)) return
    return $ \file -> do
        need [file]
        liftIO $ join $ modifyVar var $ \mp -> case Map.lookup file mp of
            Just v -> return (mp, run =<< waitBarrier v)
            Nothing -> do
                v <- newBarrier
                return $ (,) (Map.insert file v mp) $ do
                    res <- try $ act file
                    signalBarrier v res
                    run res


-- | Given a way of loading information from a file, produce a cached version that will load each file at most once.
--   Using the cached function will still result in a dependency on the original file.
--   The argument function should not access any files other than the one passed as its argument.
--   Each call to 'newCache' creates a separate cache that is independent of all other calls to 'newCache'.
--
--   This function is useful when creating files that store intermediate values,
--   to avoid the overhead of repeatedly reading from disk, particularly if the file requires expensive parsing.
--   As an example:
--
-- @
-- digits \<- 'newCache' $ \\file -> do
--     src \<- readFile file
--     return $ length $ filter isDigit src
-- \"*.digits\" '*>' \\x -> do
--     v1 \<- digits ('dropExtension' x)
--     v2 \<- digits ('dropExtension' x)
--     'Development.Shake.writeFile'' x $ show (v1,v2)
-- @
--
--   To create the result @MyFile.txt.digits@ the file @MyFile.txt@ will be read and counted, but only at most
--   once per execution.
newCache :: (FilePath -> IO a) -> Rules (FilePath -> Action a)
newCache = rulesIO . newCacheIO
