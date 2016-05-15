{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Development.Shake.Rules.File(
    need, needBS, needed, neededBS, needNorm, want,
    trackRead, trackWrite, trackAllow,
    defaultRuleFile,
    (%>), (|%>), (?>), phony, (~>), phonys,
    -- * Internal only
    -- EqualCost(..), FileQ(..), FileA, missingFile, storedValue, equalValue
    ) where

import GHC.Generics
import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.Directory
import Data.Binary
import Data.Dynamic
import Data.Maybe
import Data.List
import Data.Tuple.Extra
import System.FilePath(takeDirectory) -- important that this is the system local filepath, or wrong slashes go wrong

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as Set

import Development.Shake.Core2 hiding (trackAllow)
import Development.Shake.Core hiding (trackAllow)
import qualified Development.Shake.Core as S
import General.String
import Development.Shake.ByteString
import Development.Shake.Classes
import Development.Shake.FilePath(toStandard)
import Development.Shake.FilePattern
import Development.Shake.FileInfo
import Development.Shake.Types
import Development.Shake.Errors
import Development.Shake.Value

import Prelude hiding (mod)

infix 1 %>, ?>, |%>, ~>

newtype FileQ = FileQ {fromFileQ :: BSU}
    deriving (Typeable,Eq,Hashable,Binary,NFData,Generic)

instance Show FileQ where show (FileQ x) = unpackU x

data FileA = FileA { mod :: {-# UNPACK #-} !ModTime, size :: {-# UNPACK #-} !FileSize, digest :: {-# UNPACK #-} !FileHash }
    deriving (Typeable,Eq,Show,Generic)

instance Hashable FileA
instance NFData FileA
instance Binary FileA

type FileRule = FileQ -> Maybe FileResult

data FileResult = Root
  { act :: Action ()
  , help :: String
  } | Phony { act :: Action () }

missingFile :: FileA
missingFile = FileA FileNeq FileNeq FileNeq

-- | Interleaved file checking and result comparison, where the first is the stored result
--   and the second is the new result if known. True if the two FileA's are equal.
compareFileA :: Change -> FileQ -> Maybe FileA -> Maybe FileA -> IO Bool
compareFileA _ _ Nothing _ = return False
compareFileA _ _ (Just vo) (Just vn) | vo == vn = return True
compareFileA sC (FileQ x) (Just oldinfo@(FileA ytime ysize yhash)) vn = do
    res <- maybe (getFileInfo x) (return . Just . (mod &&& size)) vn
    case res of
        Nothing -> return $ oldinfo == missingFile
        Just (time,size) -> let m = time =?= ytime in case sC of
            ChangeModtime -> return m
            ChangeModtimeAndDigest | not m -> return False
            ChangeModtimeOrDigest | m -> return True
            _ | not (size =?= ysize) -> return False
            _ -> (=?=) yhash <$> maybe (getFileHash x) (return . digest) vn

-- | Gets the current status of the file. Also does error checking for
--   if the file wasn't created by the user rule.
getFileA :: Change -> Maybe String -> FileQ -> IO FileA
getFileA sC msg' (FileQ x) = getFileInfo x >>= \res -> case res of
    Nothing | Just msg <- msg' -> error $ msg ++ "\n  " ++ unpackU x
    Nothing -> return missingFile
    Just (time,size) -> case sC of
        ChangeModtime -> return (FileA time size FileNeq)
        _ -> FileA time size <$> getFileHash x

-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleFile :: Rules ()
defaultRuleFile = addBuiltinRule $ \x vo dep -> do
    opts@ShakeOptions{..} <- getShakeOptions
    urule <- join $ liftIO . traverse (userRule ($x) x) <$> (getUserRules :: Action (Maybe (UserRule FileRule)))
    let output = isJust urule
        isPhony = case urule of { Just (Phony{}) -> True; _ -> False}
    let sC = case shakeChange of
            ChangeModtimeAndDigestInput | output -> ChangeModtime
                                        | otherwise -> ChangeModtimeAndDigest
            x -> x
    uptodate <- case () of
        _ | shakeRunCommands == RunMinimal || not output -> return True
        _ | dep || isPhony -> return False
        _ | not shakeOutputCheck -> return True
        _ -> liftIO $ compareFileA sC x vo Nothing
    liftIO $ createDirectoryIfMissing True $ takeDirectory (unpackU . fromFileQ $ x)
    when (not uptodate) $ act (fromJust urule)
    let msg | not shakeCreationCheck && output = Nothing
            | Just Root{..} <- urule
                = Just $ "Error, rule " ++ help ++ " failed to build file:"
            | otherwise = Just "Error, file does not exist and no rule available:"
    fileA <- if isPhony then return missingFile else liftIO $ getFileA sC msg x
    changedB <- liftIO $ compareFileA sC x vo (Just fileA)
    return $ (fileA, changedB, uptodate)

-- | Main constructor for file-based rules
root :: String -> (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
root help test act = addUserRule $ \(FileQ x_) -> let x = unpackU x_ in if not $ test x then Nothing else Just $ Root { act = act x, help = help }

-- | A predicate version of 'phony', return 'Just' with the 'Action' for the matching rules.
phonys :: (String -> Maybe (Action ())) -> Rules ()
phonys act = addUserRule $ \(FileQ x_) -> fmap Phony . act $ unpackU x_


-- | Add a dependency on the file arguments, ensuring they are built before continuing.
--   The file arguments may be built in parallel, in any order. This function is particularly
--   necessary when calling 'Development.Shake.cmd' or 'Development.Shake.command'. As an example:
--
-- @
-- \"\/\/*.rot13\" '%>' \\out -> do
--     let src = 'Development.Shake.FilePath.dropExtension' out
--     'need' [src]
--     'Development.Shake.cmd' \"rot13\" [src] \"-o\" [out]
-- @
--
--   Usually @need [foo,bar]@ is preferable to @need [foo] >> need [bar]@ as the former allows greater
--   parallelism, while the latter requires @foo@ to finish building before starting to build @bar@.
--
--   This function should not be called with wildcards (e.g. @*.txt@ - use 'getDirectoryFiles' to expand them),
--   environment variables (e.g. @$HOME@ - use 'getEnv' to expand them) or directories (directories cannot be
--   tracked directly - track files within the directory instead).
need :: [FilePath] -> Action ()
need xs = (apply $ map (FileQ . packU_ . filepathNormalise . unpackU_ . packU) xs :: Action [FileA]) >> return ()

needNorm :: [FilePath] -> Action ()
needNorm xs = (apply $ map (FileQ . packU) xs :: Action [FileA]) >> return ()

needBS :: [BS.ByteString] -> Action ()
needBS xs = (apply $ map (FileQ . packU_ . filepathNormalise) xs :: Action [FileA]) >> return ()


-- | Like 'need', but if 'shakeLint' is set, check that the file does not rebuild.
--   Used for adding dependencies on files that have already been used in this rule.
needed :: [FilePath] -> Action ()
needed = neededCheck . map packU


neededBS :: [BS.ByteString] -> Action ()
neededBS = neededCheck . map packU_


neededCheck :: [BSU] -> Action ()
neededCheck = undefined
{-
neededCheck (map (FileQ . packU_ . filepathNormalise . unpackU_) -> xs) = do
    opts <- getShakeOptions
    if shakeLint opts == LintNothing then (apply xs :: Action [FileA]) >> return () else do
        pre <- liftIO $ mapM (storedValue opts) xs
        post <- apply xs :: Action [FileA]
        let bad = [ (x, case a of { Just _ -> "File change"; Nothing -> "File created" })
                  | (x, a, b) <- zip3 xs pre post, case a of { Nothing -> NotEqual; Just a -> equalValue opts a b } == NotEqual]
        case bad of
            [] -> return ()
            (file,msg):_ -> liftIO $ errorStructured
                "Lint checking error - 'needed' file required rebuilding"
                [("File", Just . unpackU . fromFileQ $ file)
                ,("Error",Just msg)]
                ""
                -}

-- | Track that a file was read by the action preceeding it. If 'shakeLint' is activated
--   then these files must be dependencies of this rule. Calls to 'trackRead' are
--   automatically inserted in 'LintFSATrace' mode.
trackRead :: [FilePath] -> Action ()
trackRead = mapM_ (trackUse . FileQ . packU)

-- | Track that a file was written by the action preceeding it. If 'shakeLint' is activated
--   then these files must either be the target of this rule, or never referred to by the build system.
--   Calls to 'trackWrite' are automatically inserted in 'LintFSATrace' mode.
trackWrite :: [FilePath] -> Action ()
trackWrite = mapM_ (trackChange . FileQ . packU)

-- | Allow accessing a file in this rule, ignoring any 'trackRead'\/'trackWrite' calls matching
--   the pattern.
trackAllow :: [FilePattern] -> Action ()
trackAllow ps = S.trackAllow $ \(FileQ x) -> any (?== unpackU x) ps


-- | Require that the argument files are built by the rules, used to specify the target.
--
-- @
-- main = 'Development.Shake.shake' 'shakeOptions' $ do
--    'want' [\"Main.exe\"]
--    ...
-- @
--
--   This program will build @Main.exe@, given sufficient rules. All arguments to all 'want' calls
--   may be built in parallel, in any order.
--
--   This function is defined in terms of 'action' and 'need', use 'action' if you need more complex
--   targets than 'want' allows.
want :: [FilePath] -> Rules ()
want [] = return ()
want xs = action $ need xs

-- | Declare a Make-style phony action.  A phony target does not name
--   a file (despite living in the same namespace as file rules);
--   rather, it names some action to be executed when explicitly
--   requested.  You can demand 'phony' rules using 'want'. (And 'need',
--   although that's not recommended.)
--
--   Phony actions are intended to define recipes that can be executed
--   by the user. If you 'need' a phony action in a rule then every
--   execution where that rule is required will rerun both the rule and
--   the phony action.  However, note that phony actions are never
--   executed more than once in a single build run.
--
--   In make, the @.PHONY@ attribute on non-file-producing rules has a
--   similar effect.  However, while in make it is acceptable to omit
--   the @.PHONY@ attribute as long as you don't create the file in
--   question, a Shake rule which behaves this way will fail lint.
--   Use a phony rule!  For file-producing rules which should be
--   rerun every execution of Shake, see 'Development.Shake.alwaysRerun'.
phony :: String -> Action () -> Rules ()
phony (toStandard -> name) act = phonys $ \s -> if s == name then Just act else Nothing

-- | Infix operator alias for 'phony', for sake of consistency with normal
--   rules.
(~>) :: String -> Action () -> Rules ()
(~>) = phony


-- | Define a rule to build files. If the first argument returns 'True' for a given file,
--   the second argument will be used to build it. Usually '%>' is sufficient, but '?>' gives
--   additional power. For any file used by the build system, only one rule should return 'True'.
--   This function will create the directory for the result file, if necessary.
--
-- @
-- (all isUpper . 'Development.Shake.FilePath.takeBaseName') '?>' \\out -> do
--     let src = 'Development.Shake.FilePath.replaceBaseName' out $ map toLower $ takeBaseName out
--     'Development.Shake.writeFile'' out . map toUpper =<< 'Development.Shake.readFile'' src
-- @
--
--   If the 'Action' completes successfully the file is considered up-to-date, even if the file
--   has not changed.
(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) test act = priority 0.5 $ root "with ?>" test act


-- | Define a set of patterns, and if any of them match, run the associated rule. Defined in terms of '%>'.
--   Think of it as the OR (@||@) equivalent of '%>'.
(|%>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(|%>) pats act = do
    let (simp,other) = partition simple pats
    case simp of
        [] -> return ()
        [p] -> let pp = toStandard p in root "with |%>" (\x -> toStandard x == pp) act
        ps -> let ps = Set.fromList $ map toStandard pats in root "with |%>" (flip Set.member ps . toStandard) act
    unless (null other) $
        let ps = map (?==) other in priority 0.5 $ root "with |%>" (\x -> any ($ x) ps) act

-- | Define a rule that matches a 'FilePattern', see '?==' for the pattern rules.
--   Patterns with no wildcards have higher priority than those with wildcards, and no file
--   required by the system may be matched by more than one pattern at the same priority
--   (see 'priority' and 'alternatives' to modify this behaviour).
--   This function will create the directory for the result file, if necessary.
--
-- @
-- \"*.asm.o\" '%>' \\out -> do
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
--
--   If the 'Action' completes successfully the file is considered up-to-date, even if the file
--   has not changed.
(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(%>) test act = (if simple test then id else priority 0.5) $ root (show test) (test ?==) act
