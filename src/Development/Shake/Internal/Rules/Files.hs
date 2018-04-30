{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, TypeFamilies #-}

module Development.Shake.Internal.Rules.Files(
    (&?>), (&%>), defaultRuleFiles
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List.Extra
import Control.Applicative
import Data.Typeable.Extra
import General.Binary

import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Errors
import General.Extra
import Development.Shake.Internal.FileName
import Development.Shake.Classes
import Development.Shake.Internal.Rules.Rerun
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.FilePattern
import Development.Shake.FilePath
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.Options
import Data.Monoid
import Prelude


infix 1 &?>, &%>


type instance RuleResult FilesQ = FilesA

newtype FilesQ = FilesQ {fromFilesQ :: [FileQ]}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

newtype FilesA = FilesA [FileA]
    deriving (Typeable,BinaryEx,NFData)

instance Show FilesA where show (FilesA xs) = unwords $ "Files" : map (drop 5 . show) xs

instance Show FilesQ where show (FilesQ xs) = unwords $ map (wrapQuote . show) xs


filesStoredValue :: ShakeOptions -> FilesQ -> IO (Maybe FilesA)
filesStoredValue opts (FilesQ xs) = fmap FilesA . sequence <$> mapM (fileStoredValue opts) xs

filesEqualValue :: ShakeOptions -> FilesA -> FilesA -> EqualCost
filesEqualValue opts (FilesA xs) (FilesA ys)
    | length xs /= length ys = NotEqual
    | otherwise = foldr and_ EqualCheap $ zipWith (fileEqualValue opts) xs ys
        where and_ NotEqual x = NotEqual
              and_ EqualCheap x = x
              and_ EqualExpensive x = if x == NotEqual then NotEqual else EqualExpensive

defaultRuleFiles :: Rules ()
defaultRuleFiles = do
    opts <- getShakeOptionsRules
    -- A rule from FilesQ to FilesA. The result value is only useful for linting.
    addBuiltinRuleEx (ruleLint opts) (ruleIdentity opts) (ruleRun opts $ shakeRebuildApply opts)

ruleLint :: ShakeOptions -> BuiltinLint FilesQ FilesA
ruleLint opts k (FilesA []) = return Nothing -- in the case of disabling lint
ruleLint opts k v = do
    now <- filesStoredValue opts k
    return $ case now of
        Nothing -> Just "<missing>"
        Just now | filesEqualValue opts v now == EqualCheap -> Nothing
                 | otherwise -> Just $ show now

ruleIdentity :: ShakeOptions -> BuiltinIdentity FilesQ FilesA
ruleIdentity opts | shakeChange opts == ChangeModtime = throwImpure $ errorStructured
    "Cannot use shakeChange=ChangeModTime with shakeShare" [] ""
ruleIdentity opts = \k (FilesA files) ->
    runBuilder $ putExList [putExStorable size <> putExStorable hash | FileA _ size hash <- files]



ruleRun :: ShakeOptions -> (FilePath -> Rebuild) -> BuiltinRun FilesQ FilesA
ruleRun opts rebuildFlags k o@(fmap getEx -> old) mode = do
    let r = map (rebuildFlags . fileNameToString . fromFileQ) $ fromFilesQ k
    case old of
        _ | RebuildNow `elem` r -> rebuild
        _ | RebuildLater `elem` r -> case old of
            Just old ->
                -- ignoring the currently stored value, which may trigger lint has changed
                -- so disable lint on this file
                return $ RunResult ChangedNothing (fromJust o) $ FilesA []
            Nothing -> do
                -- i don't have a previous value, so assume this is a source node, and mark rebuild in future
                now <- liftIO $ filesStoredValue opts k
                case now of
                    Nothing -> rebuild
                    Just now -> do alwaysRerun; return $ RunResult ChangedStore (runBuilder $ putEx now) now
        Just old | mode == RunDependenciesSame -> do
            v <- liftIO $ filesStoredValue opts k
            case v of
                Just v -> case filesEqualValue opts old v of
                    NotEqual -> rebuild
                    EqualCheap -> return $ RunResult ChangedNothing (fromJust o) v
                    EqualExpensive -> return $ RunResult ChangedStore (runBuilder $ putEx v) v
                Nothing -> rebuild
        _ -> rebuild
    where
        result v = do
            let c | Just old <- old, filesEqualValue opts old v /= NotEqual = ChangedRecomputeSame
                  | otherwise = ChangedRecomputeDiff
            return $ RunResult c (runBuilder $ putEx v) v

        rebuild = do
            putWhen Chatty $ "# " ++ show k
            (ver, act) <- getUserRuleOne k (const Nothing) ($ k)
            cache <- historyLoad k ver
            case cache of
                Just res -> do
                    v <- fmap FilesA $ forM (zip (getExList res) (fromFilesQ k)) $ \(bin, file) -> do
                        Just (FileA mod size _) <- liftIO $ fileStoredValue opts file
                        return $ FileA mod size $ getExStorable bin
                    result v
                Nothing -> do
                    FilesA v <- act
                    producesUnchecked $ map (fileNameToString . fromFileQ) $ fromFilesQ k
                    historySave k ver $ runBuilder $ putExList
                        [if isNoFileHash hash then throwImpure errorNoHash else putExStorable hash | FileA _ _ hash <- v]
                    result $ FilesA v



-- | Define a rule for building multiple files at the same time.
--   Think of it as the AND (@&&@) equivalent of '%>'.
--   As an example, a single invocation of GHC produces both @.hi@ and @.o@ files:
--
-- @
-- [\"*.o\",\"*.hi\"] '&%>' \\[o,hi] -> do
--     let hs = o 'Development.Shake.FilePath.-<.>' \"hs\"
--     'Development.Shake.need' ... -- all files the .hs import
--     'Development.Shake.cmd' \"ghc -c\" [hs]
-- @
--
--   However, in practice, it's usually easier to define rules with '%>' and make the @.hi@ depend
--   on the @.o@. When defining rules that build multiple files, all the 'FilePattern' values must
--   have the same sequence of @\/\/@ and @*@ wildcards in the same order.
--   This function will create directories for the result files, if necessary.
(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
[p] &%> act = p %> act . return
ps &%> act
    | not $ compatible ps = error $ unlines $
        "All patterns to &%> must have the same number and position of // and * wildcards" :
        ["* " ++ p ++ (if compatible [p, head ps] then "" else " (incompatible)") | p <- ps]
    | otherwise = do
        forM_ (zip [0..] ps) $ \(i,p) ->
            (if simple p then id else priority 0.5) $
                fileForward $ let op = (p ?==) in \file -> if not $ op file then Nothing else Just $ do
                    FilesA res <- apply1 $ FilesQ $ map (FileQ . fileNameFromString . substitute (extract p file)) ps
                    return $ if null res then Nothing else Just $ res !! i
        (if all simple ps then id else priority 0.5) $
            addUserRule $ \(FilesQ xs_) -> let xs = map (fileNameToString . fromFileQ) xs_ in
                if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ do
                    liftIO $ mapM_ createDirectoryRecursive $ nubOrd $ map takeDirectory xs
                    trackAllow xs
                    act xs
                    getFileTimes "&%>" xs_


-- | Define a rule for building multiple files at the same time, a more powerful
--   and more dangerous version of '&%>'. Think of it as the AND (@&&@) equivalent of '?>'.
--
--   Given an application @test &?> ...@, @test@ should return @Just@ if the rule applies, and should
--   return the list of files that will be produced. This list /must/ include the file passed as an argument and should
--   obey the invariant:
--
-- > forAll $ \x ys -> test x == Just ys ==> x `elem` ys && all ((== Just ys) . test) ys
--
--   Intuitively, the function defines a set partitioning, mapping each element to the partition that contains it.
--   As an example of a function satisfying the invariaint:
--
-- @
-- test x | 'Development.Shake.FilePath.takeExtension' x \`elem\` [\".hi\",\".o\"]
--        = Just ['Development.Shake.FilePath.dropExtension' x 'Development.Shake.FilePath.<.>' \"hi\", 'Development.Shake.FilePath.dropExtension' x 'Development.Shake.FilePath.<.>' \"o\"]
-- test _ = Nothing
-- @
--
--   Regardless of whether @Foo.hi@ or @Foo.o@ is passed, the function always returns @[Foo.hi, Foo.o]@.
(&?>) :: (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
(&?>) test act = priority 0.5 $ do
    let inputOutput suf inp out =
            ["Input" ++ suf ++ ":", "  " ++ inp] ++
            ["Output" ++ suf ++ ":"] ++ map ("  "++) out
    let normTest = fmap (map $ toStandard . normaliseEx) . test
    let checkedTest x = case normTest x of
            Nothing -> Nothing
            Just ys | x `notElem` ys -> error $ unlines $
                "Invariant broken in &?>, did not return the input (after normalisation)." :
                inputOutput "" x ys
            Just ys | bad:_ <- filter ((/= Just ys) . normTest) ys -> error $ unlines $
                ["Invariant broken in &?>, not equalValue for all arguments (after normalisation)."] ++
                inputOutput "1" x ys ++
                inputOutput "2" bad (fromMaybe ["Nothing"] $ normTest bad)
            Just ys -> Just ys

    fileForward $ \x -> case checkedTest x of
        Nothing -> Nothing
        Just ys -> Just $ do
            FilesA res <- apply1 $ FilesQ $ map (FileQ . fileNameFromString) ys
            return $ if null res then Nothing else Just $ res !! fromJust (elemIndex x ys)

    addUserRule $ \(FilesQ xs_) -> let xs@(x:_) = map (fileNameToString . fromFileQ) xs_ in
        case checkedTest x of
            Just ys | ys == xs -> Just $ do
                liftIO $ mapM_ createDirectoryRecursive $ nubOrd $ map takeDirectory xs
                act xs
                getFileTimes "&?>" xs_
            Just ys -> error $ "Error, &?> is incompatible with " ++ show xs ++ " vs " ++ show ys
            Nothing -> Nothing


getFileTimes :: String -> [FileQ] -> Action FilesA
getFileTimes name xs = do
    opts <- getShakeOptions
    let opts2 = if shakeChange opts == ChangeModtimeAndDigestInput then opts{shakeChange=ChangeModtime} else opts
    ys <- liftIO $ mapM (fileStoredValue opts2) xs
    case sequence ys of
        Just ys -> return $ FilesA ys
        Nothing | not $ shakeCreationCheck opts -> return $ FilesA []
        Nothing -> do
            let missing = length $ filter isNothing ys
            error $ "Error, " ++ name ++ " rule failed to produce " ++ show missing ++
                    " file" ++ (if missing == 1 then "" else "s") ++ " (out of " ++ show (length xs) ++ ")" ++
                    concat ["\n  " ++ fileNameToString x ++ if isNothing y then " - MISSING" else "" | (FileQ x,y) <- zip xs ys]
