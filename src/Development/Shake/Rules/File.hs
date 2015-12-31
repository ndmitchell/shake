{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Development.Shake.Rules.File(
    need, needBS, needed, neededBS, needNorm, want,
    trackRead, trackWrite, trackAllow,
    defaultRuleFile,
    (%>), (|%>), (?>), phony, (~>), phonys,
    -- * Internal only
    FileQ(..), FileA
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.Directory
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as Set

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

import Data.Bits
import Data.List
import Data.Maybe
import System.FilePath(takeDirectory) -- important that this is the system local filepath, or wrong slashes go wrong
import System.IO.Unsafe(unsafeInterleaveIO)


infix 1 %>, ?>, |%>, ~>


newtype FileQ = FileQ {fromFileQ :: BSU}
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show FileQ where show (FileQ x) = unpackU x

data FileA = FileA {-# UNPACK #-} !ModTime {-# UNPACK #-} !FileSize FileHash
    deriving (Typeable,Eq)

instance Hashable FileA where
    hashWithSalt salt (FileA a b c) = hashWithSalt salt a `xor` hashWithSalt salt b `xor` hashWithSalt salt c

instance NFData FileA where
    rnf (FileA a b c) = rnf a `seq` rnf b `seq` rnf c

instance Binary FileA where
    put (FileA a b c) = put a >> put b >> put c
    get = liftA3 FileA get get get

instance Show FileA where
    show (FileA m s h) = "File {mod=" ++ show m ++ ",size=" ++ show s ++ ",digest=" ++ show h ++ "}"

instance Rule FileQ FileA where
    storedValue ShakeOptions{shakeChange=c} (FileQ x) = do
        res <- getFileInfo x
        case res of
            Nothing -> return Nothing
            Just (time,size) | c == ChangeModtime -> return $ Just $ FileA time size fileInfoNeq
            Just (time,size) -> do
                hash <- unsafeInterleaveIO $ getFileHash x
                return $ Just $ FileA (if c == ChangeDigest then fileInfoNeq else time) size hash

    equalValue ShakeOptions{shakeChange=c} q (FileA x1 x2 x3) (FileA y1 y2 y3) = case c of
        ChangeModtime -> bool $ x1 == y1
        ChangeDigest -> bool $ x2 == y2 && x3 == y3
        ChangeModtimeOrDigest -> bool $ x1 == y1 && x2 == y2 && x3 == y3
        _ | x1 == y1 -> EqualCheap
          | x2 == y2 && x3 == y3 -> EqualExpensive
          | otherwise -> NotEqual
        where bool b = if b then EqualCheap else NotEqual


-- | Arguments: options; is the file an input; a message for failure if the file does not exist; filename
storedValueError :: ShakeOptions -> Bool -> String -> FileQ -> IO FileA
{-
storedValueError opts False msg x | False && not (shakeOutputCheck opts) = do
    when (shakeCreationCheck opts) $ do
        whenM (isNothing <$> (storedValue opts x :: IO (Maybe FileA))) $ error $ msg ++ "\n  " ++ unpackU (fromFileQ x)
    return $ FileA fileInfoEq fileInfoEq fileInfoEq
-}
storedValueError opts input msg x = fromMaybe def <$> storedValue opts2 x
    where def = if shakeCreationCheck opts || input then error err else FileA fileInfoNeq fileInfoNeq fileInfoNeq
          err = msg ++ "\n  " ++ unpackU (fromFileQ x)
          opts2 = if not input && shakeChange opts == ChangeModtimeAndDigestInput then opts{shakeChange=ChangeModtime} else opts


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleFile :: Rules ()
defaultRuleFile = priority 0 $ rule $ \x -> Just $ do
    opts <- getShakeOptions
    liftIO $ storedValueError opts True "Error, file does not exist and no rule available:" x


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
need :: [FilePath] -> Action ()
need xs = (apply $ map (FileQ . packU_ . filepathNormalise . unpackU_ . packU) xs :: Action [FileA]) >> return ()

needNorm :: [FilePath] -> Action ()
needNorm xs = (apply $ map (FileQ . packU) xs :: Action [FileA]) >> return ()

needBS :: [BS.ByteString] -> Action ()
needBS xs = (apply $ map (FileQ . packU_ . filepathNormalise) xs :: Action [FileA]) >> return ()


-- | Like 'need', but if 'shakeLint' is set, check that the file does not rebuild.
--   Used for adding dependencies on files that have already been used in this rule.
needed :: [FilePath] -> Action ()
needed xs = do
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then need xs else neededCheck $ map packU xs


neededBS :: [BS.ByteString] -> Action ()
neededBS xs = do
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then needBS xs else neededCheck $ map packU_ xs


neededCheck :: [BSU] -> Action ()
neededCheck (map (packU_ . filepathNormalise . unpackU_) -> xs) = do
    opts <- getShakeOptions
    pre <- liftIO $ mapM (storedValue opts . FileQ) xs
    post <- apply $ map FileQ xs :: Action [FileA]
    let bad = [ (x, if isJust a then "File change" else "File created")
              | (x, a, b) <- zip3 xs pre post, maybe NotEqual (\a -> equalValue opts (FileQ x) a b) a == NotEqual]
    case bad of
        [] -> return ()
        (file,msg):_ -> liftIO $ errorStructured
            "Lint checking error - 'needed' file required rebuilding"
            [("File", Just $ unpackU file)
            ,("Error",Just msg)]
            ""


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
trackAllow ps = do
    opts <- getShakeOptions
    when (isJust $ shakeLint opts) $
        S.trackAllow $ \(FileQ x) -> any (?== unpackU x) ps


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


root :: String -> (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
root help test act = rule $ \(FileQ x_) -> let x = unpackU x_ in
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        act x
        opts <- getShakeOptions
        liftIO $ storedValueError opts False ("Error, rule " ++ help ++ " failed to build file:") $ FileQ x_


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
phony name act = phonys $ \s -> if s == name then Just act else Nothing

-- | A predicate version of 'phony', return 'Just' with the 'Action' for the matching rules.
phonys :: (String -> Maybe (Action ())) -> Rules ()
phonys act = rule $ \(FileQ x_) -> case act $ unpackU x_ of
    Nothing -> Nothing
    Just act -> Just $ do
        act
        return $ FileA fileInfoNeq fileInfoNeq fileInfoNeq

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
