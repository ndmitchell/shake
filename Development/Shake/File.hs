{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.File(
    need, want,
    defaultRuleFile,
    (*>), (**>), (?>)
    ) where

import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.List
import Data.Typeable
import System.Directory

import Development.Shake.Core
import Development.Shake.FilePattern
import Development.Shake.FileTime
import System.FilePath(takeDirectory)


newtype File = File FilePath
    deriving (Typeable,Eq,Hashable,Binary)

instance Show File where show (File x) = x


instance Rule File FileTime where
    validStored (File x) t = fmap (== Just t) $ getModTimeMaybe x


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
(**>) test act = (\x -> any (x ?==) test) ?> act

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
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(*>) test act = (test ?==) ?> act
