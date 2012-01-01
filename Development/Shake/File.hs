{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Development.Shake.File(
    need, want,
    defaultRuleFile,
    (*>), (**>), (?>)
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import System.Directory

import Development.Shake.Core
import Development.Shake.FilePath
import Development.Shake.FilePattern
import Development.Shake.FileTime


newtype File = File FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show File where show (File x) = x


instance Rule File FileTime where
    validStored (File x) t = fmap (== Just t) $ getModTimeMaybe x

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



-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleFile :: Rules ()
defaultRuleFile = defaultRule $ \(File x) -> Just $
    liftIO $ getModTimeError "Error, file does not exist and no rule available:" x


-- | Require that the following files are built before continuing. Particularly
--   necessary when calling 'system''. As an example:
--
-- > "//*.rot13" *> \out -> do
-- >     let src = dropExtension out
-- >     need [src]
-- >     system' ["rot13",src,"-o",out]
need :: [FilePath] -> Action ()
need xs = (apply $ map File xs :: Action [FileTime]) >> return ()

-- | Require that the following are built by the rules, used to specify the target.
--
-- > main = shake shakeOptions $ do
-- >    want ["Main.exe"]
-- >    ...
--
--   This program will build @Main.exe@, given sufficient rules.
want :: [FilePath] -> Rules ()
want xs = action $ need xs


-- | Define a rule to build files. If the first argument returns 'True' for a given file,
--   the second argument will be used to build it. Usually '*>' is sufficient, but '?>' gives
--   additional power. For any file used by the build system, only one rule should return 'True'.
--
-- > (all isUpper . takeBaseName) *> \out -> do
-- >     let src = replaceBaseName out $ map toLower $ takeBaseName out
-- >     writeFile' . map toUpper =<< readFile' src
(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) test act = rule $ \(File x) ->
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        act x
        liftIO $ getModTimeError "Error, rule failed to build the file:" x


-- | Define a set of patterns, and if any of them match, run the associated rule. See '*>'.
(**>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(**>) test act = (\x -> any (?== x) test) ?> act

-- | Define a rule that matches a 'FilePattern'. No file required by the system must be
--   matched by more than one pattern. For the pattern rules, see '?=='.
--
-- > "*.asm.o" *> \out -> do
-- >     let src = dropExtension out
-- >     need [src]
-- >     system' ["as",src,"-o",out]
--
--   To define a build system for multiple compiled languages, we recommend using @.asm.o@,
--   @.cpp.o@, @.hs.o@, to indicate which language produces an object file.
--   I.e., the file @foo.cpp@ produces object file @foo.cpp.o@.
--
--   Note that matching is case-sensitive, even on Windows.
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(*>) test act = (test ?==) ?> act
