{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, ConstraintKinds, ViewPatterns, TypeFamilies #-}

module Development.Shake.Internal.Rules.Files(
    (&?>), (&%>), defaultRuleFiles
    ) where

import Control.DeepSeq (force)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.List.Extra
import System.Directory
import Control.Applicative
import Data.Typeable.Extra
import General.Binary
import Prelude

import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Action hiding (trackAllow)
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Rules
import General.Extra
import Development.Shake.Internal.FileName
import Development.Shake.Classes
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.FilePattern
import Development.Shake.FilePath
import Development.Shake.Internal.Options


infix 1 &?>, &%>


type instance RuleResult FilesQ = FilesA

newtype FilesQ = FilesQ [FileQ]
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)



newtype FilesA = FilesA [FileA]
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show FilesA where show (FilesA xs) = unwords $ "Files" : map (drop 5 . show) xs

instance Show FilesQ where show (FilesQ xs) = unwords $ map (wrapQuote . show) xs


filesStoredValue :: ShakeOptions -> FilesQ -> IO (Maybe FilesA)
filesStoredValue opts (FilesQ xs) = (fmap FilesA . sequence) <$> mapM (fileStoredValue opts) xs

filesEqualValue :: ShakeOptions -> FilesA -> FilesA -> EqualCost
filesEqualValue opts (FilesA xs) (FilesA ys)
    | length xs /= length ys = NotEqual
    | otherwise = foldr and_ EqualCheap (zipWith (fileEqualValue opts) xs ys)
        where and_ NotEqual x = NotEqual
              and_ EqualCheap x = x
              and_ EqualExpensive x = if x == NotEqual then NotEqual else EqualExpensive

defaultRuleFiles :: Rules ()
defaultRuleFiles = do
    opts <- getShakeOptionsRules
    let (run, lint) = convertLegacy LegacyRule
            {storedValue=filesStoredValue opts
            ,equalValue=filesEqualValue opts
            ,executeRule = \(k :: FilesQ) -> do
                rules :: UserRule (FilesQ -> Maybe (Action FilesA)) <- getUserRules
                case userRuleMatch rules ($ k) of
                        [r] -> r
                        rs  -> liftIO $ errorMultipleRulesMatch (typeOf k) (show k) (length rs)
            }

    let summary (FilesQ ks) _ = do
            hashes <- forM ks $ \(FileQ k) -> do
                str <- LBS.readFile (fileNameToString k)
                return $ SHA.showDigest $ SHA.sha256 str
            return $ force $ SHA.showDigest $ SHA.sha256 $ LBS.pack $ show hashes

    addBuiltinRuleEx newBinaryOp lint summary run



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
                    return $ res !! i
        (if all simple ps then id else priority 0.5) $
            addUserRule $ \(FilesQ xs_) -> let xs = map (fileNameToString . fromFileQ) xs_ in
                if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ do
                    liftIO $ mapM_ (createDirectoryIfMissing True) $ nubOrd $ map takeDirectory xs
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
            return $ res !! fromJust (elemIndex x ys)

    addUserRule $ \(FilesQ xs_) -> let xs@(x:_) = map (fileNameToString . fromFileQ) xs_ in
        case checkedTest x of
            Just ys | ys == xs -> Just $ do
                liftIO $ mapM_ (createDirectoryIfMissing True) $ nubOrd $ map takeDirectory xs
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
            error $ "Error, " ++ name ++ " rule failed to build " ++ show missing ++
                    " file" ++ (if missing == 1 then "" else "s") ++ " (out of " ++ show (length xs) ++ ")" ++
                    concat ["\n  " ++ fileNameToString x ++ if isNothing y then " - MISSING" else "" | (FileQ x,y) <- zip xs ys]


data LegacyRule key value = LegacyRule
    {storedValue :: key -> IO (Maybe value)
        -- ^ /[Required]/ Retrieve the @value@ associated with a @key@, if available.
        --
        --   As an example for filenames/timestamps, if the file exists you should return 'Just'
        --   the timestamp, but otherwise return 'Nothing'.
    ,equalValue :: value -> value -> EqualCost
        -- ^ /[Optional]/ Equality check, with a notion of how expensive the check was.
        --   Use 'defaultLegacyRule' if you do not want a different equality.
    ,executeRule :: key -> Action value
        -- ^ How to run a rule, given ways to get a UserRule.
    }


convertLegacy :: forall k v . (BinaryEx v, ShakeValue k, ShakeValue v) => LegacyRule k v -> (BuiltinRun k v, BuiltinLint k v)
convertLegacy LegacyRule{..} = (builtinRun, builtinLint)
    where
        builtinLint k v = do
            now <- storedValue k
            return $ case now of
                Nothing -> Just "<missing>"
                Just now | equalValue v now == EqualCheap -> Nothing
                         | otherwise -> Just $ show now

        builtinRun k (fmap getEx -> old) dirtyChildren = case old of
                Just old | not dirtyChildren -> do
                    v <- liftIO $ storedValue k
                    case v of
                        Just v -> do
                            let e = equalValue old v
                            case e of
                                NotEqual -> rebuild k $ Just old
                                EqualCheap -> return $ RunResult ChangedNothing (runBuilder $ putEx v) v
                                EqualExpensive -> return $ RunResult ChangedStore (runBuilder $ putEx v) v
                        Nothing -> rebuild k $ Just old
                _ -> rebuild k old
            where
                rebuild k old = do
                    putWhen Chatty $ "# " ++ show k
                    v <- executeRule k
                    let c | Just old <- old, equalValue old v /= NotEqual = ChangedRecomputeSame
                          | otherwise = ChangedRecomputeDiff
                    return $ RunResult c (runBuilder $ putEx v) v
