{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, RecordWildCards, FlexibleInstances, TypeFamilies, ConstraintKinds #-}

module Development.Shake.Internal.Rules.File(
    need, needHasChanged, needBS, needed, neededBS, want,
    trackRead, trackWrite, trackAllow,
    defaultRuleFile,
    (%>), (|%>), (?>), phony, (~>), phonys,
    resultHasChanged,
    -- * Internal only
    FileQ(..), FileA(..), fileStoredValue, fileEqualValue, EqualCost(..), fileForward
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Typeable.Extra
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as Set
import Foreign.Storable
import Data.Word
import Data.Monoid
import General.Binary
import General.Extra

import Development.Shake.Internal.Core.Types hiding (Result, result)
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.FileName
import Development.Shake.Internal.Rules.Rerun
import Development.Shake.Classes
import Development.Shake.FilePath(toStandard)
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors

import System.FilePath(takeDirectory) -- important that this is the system local filepath, or wrong slashes go wrong
import System.IO.Unsafe(unsafeInterleaveIO)

import Prelude


infix 1 %>, ?>, |%>, ~>

---------------------------------------------------------------------
-- TYPES

type instance RuleResult FileQ = FileR

-- | The unique key we use to index File rules, to avoid name clashes.
newtype FileQ = FileQ {fromFileQ :: FileName}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

-- | Raw information about a file.
data FileA = FileA {-# UNPACK #-} !ModTime {-# UNPACK #-} !FileSize FileHash
    deriving (Typeable)

-- | Result of a File rule, may contain raw file information and whether the rule did run this build
data FileR = FileR { result :: !(Maybe FileA) -- ^ Raw information about the file built by this rule.
                                           --   Set to 'Nothing' to prevent linting some times.
                   , useLint :: !Bool       -- ^ Should we lint the resulting file
                   , hasChanged :: !Bool    -- ^ Whether the file changed this build. Transient
                                           --   information, that doesn't get serialized.
                   }
    deriving (Typeable)

-- | The types of file rule that occur.
data Mode
    = ModePhony (Action ()) -- ^ An action with no file value
    | ModeDirect (Action ()) -- ^ An action that produces this file
    | ModeForward (Action (Maybe FileA)) -- ^ An action that looks up a file someone else produced

-- | The results of the various 'Mode' rules.
data Result
    = ResultPhony
    | ResultDirect FileA
    | ResultForward FileA

-- | The file rules we use, first is the name (as pretty as you can get).
data FileRule = FileRule String (FilePath -> Maybe Mode)
    deriving Typeable


---------------------------------------------------------------------
-- INSTANCES

instance Show FileQ where show (FileQ x) = fileNameToString x

instance BinaryEx [FileQ] where
    putEx = putEx . map fromFileQ
    getEx = map FileQ . getEx

instance NFData FileA where
    rnf (FileA a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData FileR where
    rnf (FileR a b c) = rnf a `seq` rnf b `seq` rnf c

instance Show FileA where
    show (FileA m s h) = "File {mod=" ++ show m ++ ",size=" ++ show s ++ ",digest=" ++ show h ++ "}"

instance Show FileR where
    show FileR{..} = show result ++ if hasChanged then " recomputed" else " not recomputed"

instance Storable FileA where
    sizeOf _ = 4 * 3 -- 4 Word32's
    alignment _ = alignment (undefined :: ModTime)
    peekByteOff p i = FileA <$> peekByteOff p i <*> peekByteOff p (i+4) <*> peekByteOff p (i+8)
    pokeByteOff p i (FileA a b c) = pokeByteOff p i a >> pokeByteOff p (i+4) b >> pokeByteOff p (i+8) c

instance BinaryEx FileA where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx [FileA] where
    putEx = putExStorableList
    getEx = getExStorableList

fromResult :: Result -> Maybe FileA
fromResult ResultPhony = Nothing
fromResult (ResultDirect x) = Just x
fromResult (ResultForward x) = Just x

instance BinaryEx Result where
    putEx ResultPhony = mempty
    putEx (ResultDirect x) = putEx x
    putEx (ResultForward x) = putEx (0 :: Word8) <> putEx x

    getEx x = case BS.length x of
        0 -> ResultPhony
        12 -> ResultDirect $ getEx x
        13 -> ResultForward $ getEx $ BS.tail x


---------------------------------------------------------------------
-- FILE CHECK QUERIES

-- | An equality check and a cost.
data EqualCost
    = EqualCheap -- ^ The equality check was cheap.
    | EqualExpensive -- ^ The equality check was expensive, as the results are not trivially equal.
    | NotEqual -- ^ The values are not equal.
      deriving (Eq,Ord,Show,Read,Typeable,Enum,Bounded)

fileStoredValue :: ShakeOptions -> FileQ -> IO (Maybe FileA)
fileStoredValue ShakeOptions{shakeChange=c} (FileQ x) = do
    res <- getFileInfo x
    case res of
        Nothing -> return Nothing
        Just (time,size) | c == ChangeModtime -> return $ Just $ FileA time size noFileHash
        Just (time,size) -> do
            hash <- unsafeInterleaveIO $ getFileHash x
            return $ Just $ FileA time size hash


fileEqualValue :: ShakeOptions -> FileA -> FileA -> EqualCost
fileEqualValue ShakeOptions{shakeChange=c} (FileA x1 x2 x3) (FileA y1 y2 y3) = case c of
    ChangeModtime -> bool $ x1 == y1
    ChangeDigest -> bool $ x2 == y2 && x3 == y3
    ChangeModtimeOrDigest -> bool $ x1 == y1 && x2 == y2 && x3 == y3
    _ | x1 == y1 -> EqualCheap
      | x2 == y2 && x3 == y3 -> EqualExpensive
      | otherwise -> NotEqual
    where bool b = if b then EqualCheap else NotEqual


-- | Arguments: options; is the file an input; a message for failure if the file does not exist; filename
storedValueError :: ShakeOptions -> Bool -> String -> FileQ -> IO (Maybe FileA)
{-
storedValueError opts False msg x | False && not (shakeOutputCheck opts) = do
    when (shakeCreationCheck opts) $ do
        whenM (isNothing <$> (storedValue opts x :: IO (Maybe FileA))) $ error $ msg ++ "\n  " ++ unpackU (fromFileQ x)
    return $ FileA fileInfoEq fileInfoEq fileInfoEq
-}
storedValueError opts input msg x = maybe def Just <$> fileStoredValue opts2 x
    where def = if shakeCreationCheck opts || input then error err else Nothing
          err = msg ++ "\n  " ++ fileNameToString (fromFileQ x)
          opts2 = if not input && shakeChange opts == ChangeModtimeAndDigestInput then opts{shakeChange=ChangeModtime} else opts


---------------------------------------------------------------------
-- THE DEFAULT RULE

defaultRuleFile :: Rules ()
defaultRuleFile = do
    opts@ShakeOptions{..} <- getShakeOptionsRules
    -- A rule from FileQ to (Maybe FileA). The result value is only useful for linting.
    addBuiltinRuleEx (ruleLint opts) (ruleIdentity opts) (ruleRun opts $ shakeRebuildApply opts)

ruleLint :: ShakeOptions -> BuiltinLint FileQ FileR
ruleLint opts k (FileR (Just v) True _) = do
    now <- fileStoredValue opts k
    return $ case now of
        Nothing -> Just "<missing>"
        Just now | fileEqualValue opts v now == EqualCheap -> Nothing
                 | otherwise -> Just $ show now
ruleLint _ _ _ = return Nothing

ruleIdentity :: ShakeOptions -> BuiltinIdentity FileQ FileR
ruleIdentity opts | shakeChange opts == ChangeModtime = throwImpure errorNoHash
ruleIdentity opts = \k v -> case result v of
    Just (FileA _ size hash) -> runBuilder $ putExStorable size <> putExStorable hash
    Nothing -> throwImpure $ errorInternal $ "File.ruleIdentity has no result for " ++ show k

ruleRun :: ShakeOptions -> (FilePath -> Rebuild) -> BuiltinRun FileQ FileR
ruleRun opts@ShakeOptions{..} rebuildFlags o@(FileQ x) oldBin@(fmap getEx -> old) mode = do
    -- for One, rebuild makes perfect sense
    -- for Forward, we expect the child will have already rebuilt - Rebuild just lets us deal with code changes
    -- for Phony, it doesn't make that much sense, but probably isn't harmful?
    let r = rebuildFlags $ fileNameToString x


    case old of
        _ | r == RebuildNow -> rebuild
        _ | r == RebuildLater -> case old of
            Just old ->
                -- ignoring the currently stored value, which may trigger lint has changed
                -- so disable lint on this file
                unLint <$> retOld ChangedNothing
            Nothing -> do
                -- i don't have a previous value, so assume this is a source node, and mark rebuild in future
                now <- liftIO $ fileStoredValue opts o
                case now of
                    Nothing -> rebuild
                    Just now -> do alwaysRerun; retNew ChangedStore $ ResultDirect now
        {-
        _ | r == RebuildNever -> do
            now <- liftIO $ fileStoredValue opts o
            case now of
                Nothing -> rebuild
                Just now -> do
                    let diff | Just (ResultDirect old) <- old, fileEqualValue opts old now /= NotEqual = ChangedRecomputeSame
                                | otherwise = ChangedRecomputeDiff
                    retNew diff $ ResultDirect now
        -}
        Just (ResultDirect old) | mode == RunDependenciesSame -> do
            now <- liftIO $ fileStoredValue opts o
            let noHash (FileA _ _ x) = isNoFileHash x
            case now of
                Nothing -> rebuild
                Just now -> case fileEqualValue opts old now of
                    NotEqual ->
                        rebuild
                    -- if our last build used no file hashing, but this build should, then we must refresh the hash
                    EqualCheap | if noHash old then shakeChange == ChangeModtimeAndDigestInput || noHash now else True ->
                        retOld ChangedNothing
                    _ ->
                        retNew ChangedStore $ ResultDirect now
        Just (ResultForward old) | mode == RunDependenciesSame -> retOld ChangedNothing
        _ -> rebuild
    where
        -- no need to lint check forward files
        -- but more than that, it goes wrong if you do, see #427
        fileR (ResultDirect x) = FileR (Just x) True
        fileR (ResultForward x) = FileR (Just x) False
        fileR ResultPhony = FileR Nothing False
        unLint (RunResult a b c) = RunResult a b c{useLint = False}

        retNew :: RunChanged -> Result -> Action (RunResult FileR)
        retNew c v = return $ RunResult c (runBuilder $ putEx v) $ fileR v (c == ChangedRecomputeDiff)

        retOld :: RunChanged -> Action (RunResult FileR)
        retOld c = return $ RunResult c (fromJust oldBin) $ fileR (fromJust old) False

        -- actually run the rebuild
        rebuild = do
            putWhen Chatty $ "# " ++ show o
            x <- return $ fileNameToString x
            act <- getUserRuleMaybe o (\(FileRule s _) -> Just s) $ \(FileRule _ f) -> f x
            let answer ctor new = do
                    let b = case () of
                                _ | Just old <- old
                                    , Just old <- fromResult old
                                    , fileEqualValue opts old new /= NotEqual -> ChangedRecomputeSame
                                _ -> ChangedRecomputeDiff
                    retNew b $ ctor new
            case act of
                Nothing -> do
                    new <- liftIO $ storedValueError opts True "Error, file does not exist and no rule available:" o
                    answer ResultDirect $ fromJust new
                Just (_, ModeForward act) -> do
                    new <- act
                    case new of
                        Nothing -> do
                            alwaysRerun
                            retNew ChangedRecomputeDiff ResultPhony
                        Just new -> answer ResultForward new
                Just (ver, ModeDirect act) -> do
                    cache <- historyLoad o ver
                    case cache of
                        Just encodedHash -> do
                            Just (FileA mod size _) <- liftIO $ storedValueError opts False "Error, restored the rule but did not produce file:" o
                            answer ResultDirect $ FileA mod size $ getExStorable encodedHash
                        Nothing -> do
                            act
                            new <- liftIO $ storedValueError opts False "Error, rule finished running but did not produce file:" o
                            case new of
                                Nothing -> retNew ChangedRecomputeDiff ResultPhony
                                Just new@(FileA _ fileSize fileHash) -> do
                                    producesUnchecked [x]
                                    res <- answer ResultDirect new
                                    historySave o ver $ runBuilder $
                                        if isNoFileHash fileHash then throwImpure errorNoHash else putExStorable fileHash
                                    return res
                Just (_, ModePhony act) -> do
                    -- See #523 and #524
                    -- Shake runs the dependencies first, but stops when one has changed.
                    -- We don't want to run the existing deps first if someone changes the build system,
                    -- so insert a fake dependency that cuts the process dead.
                    alwaysRerun
                    act
                    retNew ChangedRecomputeDiff ResultPhony


apply_ :: Partial => (a -> FileName) -> [a] -> Action [FileR]
apply_ f = apply . map (FileQ . f)


-- | Has a file changed. This function will only give the correct answer if called in the rule
--   producing the file, /before/ the rule has modified the file in question.
--   Best avoided, but sometimes necessary in conjunction with 'needHasChanged' to cause rebuilds
--   to happen if the result is deleted or modified.
resultHasChanged :: FilePath -> Action Bool
resultHasChanged file = do
    let filename = FileQ $ fileNameFromString file
    res <- getDatabaseValue filename
    old <- return $ case res of
        Nothing -> Nothing
        Just (Left bs) -> fromResult $ getEx bs
        Just (Right v) -> result v
    case old of
        Nothing -> return True
        Just old -> do
            opts <- getShakeOptions
            new <- liftIO $ fileStoredValue opts filename
            return $ case new of
                Nothing -> True
                Just new -> fileEqualValue opts old new == NotEqual


---------------------------------------------------------------------
-- OPTIONS ON TOP

-- | Internal method for adding forwarding actions
fileForward :: String -> (FilePath -> Maybe (Action (Maybe FileA))) -> Rules ()
fileForward help act = addUserRule $ FileRule help $ fmap ModeForward . act


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
need :: Partial => [FilePath] -> Action ()
need = withFrozenCallStack $ void . apply_ fileNameFromString


-- | Like 'need' but returns a list of rebuild dependencies this build.
--
--   The following example writes a list of changed dependencies to a file as its action.
--
-- @
-- \"target\" '%>' \\out -> do
--       let sourceList = [\"source1\", \"source2\"]
--       rebuildList <- 'needHasChanged' sourceList
--       'Development.Shake.writeFileLines' out rebuildList
-- @
--
--   This function can be used to alter the action depending on which dependency needed
--   to be rebuild.
--
--   Note that a rule can be run even if no dependency has changed, for example
--   because of 'shakeRebuild' or because the target has changed or been deleted.
--   To detect the latter case you may wish to use 'resultHasChanged'.
needHasChanged :: Partial => [FilePath] -> Action [FilePath]
needHasChanged paths = withFrozenCallStack $ do
    res <- apply_ fileNameFromString paths
    return [a | (a,b) <- zip paths res, hasChanged b]

needBS :: Partial => [BS.ByteString] -> Action ()
needBS = withFrozenCallStack $ void . apply_ fileNameFromByteString

-- | Like 'need', but if 'shakeLint' is set, check that the file does not rebuild.
--   Used for adding dependencies on files that have already been used in this rule.
needed :: Partial => [FilePath] -> Action ()
needed xs = withFrozenCallStack $ do
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then need xs else neededCheck $ map fileNameFromString xs


neededBS :: Partial => [BS.ByteString] -> Action ()
neededBS xs = withFrozenCallStack $ do
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then needBS xs else neededCheck $ map fileNameFromByteString xs


neededCheck :: Partial => [FileName] -> Action ()
neededCheck xs = withFrozenCallStack $ do
    opts <- getShakeOptions
    pre <- liftIO $ mapM (fileStoredValue opts . FileQ) xs
    post <- apply_ id xs
    let bad = [ (x, if isJust a then "File change" else "File created")
              | (x, a, FileR (Just b) _ _) <- zip3 xs pre post, maybe NotEqual (\a -> fileEqualValue opts a b) a == NotEqual]
    case bad of
        [] -> return ()
        (file,msg):_ -> throwM $ errorStructured
            "Lint checking error - 'needed' file required rebuilding"
            [("File", Just $ fileNameToString file)
            ,("Error",Just msg)]
            ""


-- | Track that a file was read by the action preceeding it. If 'shakeLint' is activated
--   then these files must be dependencies of this rule. Calls to 'trackRead' are
--   automatically inserted in 'LintFSATrace' mode.
trackRead :: [FilePath] -> Action ()
trackRead = lintTrackRead . map (FileQ . fileNameFromString)

-- | Track that a file was written by the action preceeding it. If 'shakeLint' is activated
--   then these files must either be the target of this rule, or never referred to by the build system.
--   Calls to 'trackWrite' are automatically inserted in 'LintFSATrace' mode.
trackWrite :: [FilePath] -> Action ()
trackWrite = lintTrackWrite . map (FileQ . fileNameFromString)

-- | Allow accessing a file in this rule, ignoring any 'trackRead' \/ 'trackWrite' calls matching
--   the pattern.
trackAllow :: [FilePattern] -> Action ()
trackAllow ps = lintTrackAllow $ \(FileQ x) -> any (?== fileNameToString x) ps


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
want :: Partial => [FilePath] -> Rules ()
want [] = return ()
want xs = withFrozenCallStack $ action $ need xs


root :: String -> (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
root help test act = addUserRule $ FileRule help $ \x -> if not $ test x then Nothing else Just $ ModeDirect $ do
    liftIO $ createDirectoryRecursive $ takeDirectory x
    act x


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
phony :: Located => String -> Action () -> Rules ()
phony oname@(toStandard -> name) act =
    addPhony ("phony " ++ show oname ++ " at " ++ callStackTop) $ \s -> if s == name then Just act else Nothing

-- | A predicate version of 'phony', return 'Just' with the 'Action' for the matching rules.
phonys :: Located => (String -> Maybe (Action ())) -> Rules ()
phonys = addPhony ("phonys at " ++ callStackTop)

-- | Infix operator alias for 'phony', for sake of consistency with normal
--   rules.
(~>) :: Located => String -> Action () -> Rules ()
(~>) oname@(toStandard -> name) act =
    addPhony (show oname ++ " ~> at " ++ callStackTop) $ \s -> if s == name then Just act else Nothing

addPhony :: String -> (String -> Maybe (Action ())) -> Rules ()
addPhony help act = addUserRule $ FileRule help $ fmap ModePhony . act


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
(?>) :: Located => (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) test act = priority 0.5 $ root ("?> at " ++ callStackTop) test act


-- | Define a set of patterns, and if any of them match, run the associated rule. Defined in terms of '%>'.
--   Think of it as the OR (@||@) equivalent of '%>'.
(|%>) :: Located => [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(|%>) pats act = do
    let (simp,other) = partition simple pats
    case simp of
        [] -> return ()
        [p] -> let pp = toStandard p in root help (\x -> toStandard x == pp) act
        ps -> let ps = Set.fromList $ map toStandard pats in root help (flip Set.member ps . toStandard) act
    unless (null other) $
        let ps = map (?==) other in priority 0.5 $ root help (\x -> any ($ x) ps) act
    where help = show pats ++ " |%> at " ++ callStackTop

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
(%>) :: Located => FilePattern -> (FilePath -> Action ()) -> Rules ()
(%>) test act = (if simple test then id else priority 0.5) $ root (show test ++ " %> at " ++ callStackTop) (test ?==) act
