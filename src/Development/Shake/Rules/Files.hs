{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns, RecordWildCards #-}

module Development.Shake.Rules.Files(
    (&?>), (&%>), defaultRuleFiles
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List.Extra
import System.Directory
import Prelude

import Data.Binary
import Development.Shake.Core hiding (trackAllow)
import General.Extra
import General.String
import Development.Shake.Classes
import Development.Shake.Rules.File
import Development.Shake.Rules.Rerun
import Development.Shake.FilePattern
import Development.Shake.FilePath
import Development.Shake.Types
import Development.Shake.ByteString


infix 1 &?>, &%>


newtype FilesQ = FilesQ [FileQ]
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show FilesQ where show (FilesQ xs) = unwords $ map (showQuote . show) xs

newtype FilesRule = FilesRule { fromFilesRule :: [FileQ] -> Maybe (Action ()) }
    deriving (Typeable)

defaultRuleFiles :: Rules ()
defaultRuleFiles = addBuiltinRule $ \(FilesQ xs) vo dep -> do
    opts@ShakeOptions{..} <- getShakeOptions
    outputCheck <- outputCheck
    Just urule <- userRule fromFilesRule xs
    let sC = case shakeChange of
            ChangeModtimeAndDigestInput -> ChangeModtime
            x -> x
    uptodate <- case () of
        _ | shakeRunCommands == RunMinimal -> return True
        _ | dep -> return False
        _ | not outputCheck -> return True
        _ -> maybe (return False) (fmap and . zipWithM (\x vo -> liftIO $ compareFileA sC x (Just vo) Nothing) xs) vo
    mapM_ (liftIO . createDirectoryIfMissing True . takeDirectory . unpackU . fromFileQ) xs
    when (not uptodate) $ urule
    let msg | not shakeCreationCheck = Nothing
            | otherwise = Just $ "Error, rule for " ++ show (FilesQ xs) ++ " failed to build file:"
    filesA <- mapM (liftIO . getFileA sC msg) xs
    equalF <- maybe (return False) (fmap and . mapM (\(x,vn,vo) -> liftIO $ compareFileA sC x (Just vo) Nothing) . zip3 xs filesA) vo
    return $ BuiltinResult (encode filesA) filesA (not uptodate) equalF

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
--   have the same sequence of @\/\/@ and @*@ wildcards in the same order. They are substituted to match.
--   This function will create directories for the result files, if necessary.
(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
ps &%> act
    | not $ compatible ps = error $ unlines $
        "All patterns to &%> must have the same number and position of // and * wildcards" :
        ["* " ++ p ++ (if compatible [p, head ps] then "" else " (incompatible)") | p <- ps]
    | otherwise = do
        forM_ (zip [0..] ps) $ \(i,p) -> forward p $ \file vo -> do
            let fileqs = map (FileQ . packU_ . filepathNormalise . unpackU_ . packU . substitute (extract p file)) ps
            files <- apply1 $ FilesQ $ fileqs
            let vn = files !! i
            sC <- flip fmap getShakeOptions $ \opts -> case shakeChange opts of
                ChangeModtimeAndDigestInput -> ChangeModtime
                x -> x
            equalF <- liftIO $ compareFileA sC (fileqs !! i) vo (Just vn) -- cheap (no-disk) comparison
            return $ BuiltinResult (encode vn) vn False (not equalF)
        (if all simple ps then id else priority 0.5) $
            addUserRule . FilesRule $ \xs_ -> let xs = map (unpackU . fromFileQ) xs_ in
                if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ trackAllow xs >> act xs


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
(&?>) test act = undefined{- priority 0.5 $ do
    let norm = toStandard . normaliseEx
    let inputOutput suf inp out =
            ["Input" ++ suf ++ ":", "  " ++ inp] ++
            ["Output" ++ suf ++ ":"] ++ map ("  "++) out
    let normTest = fmap (map norm) . test
    let checkedTest x = case normTest x of
            Nothing -> Nothing
            Just ys | x `notElem` ys -> error $ unlines $
                "Invariant broken in &?>, did not return the input (after normalisation)." :
                inputOutput "" x ys
            Just ys | bad:_ <- filter ((/= Just ys) . normTest) ys -> error $ unlines $
                ["Invariant broken in &?>, not equal for all arguments (after normalisation)."] ++
                inputOutput "1" x ys ++
                inputOutput "2" bad (fromMaybe ["Nothing"] $ normTest bad)
            Just ys -> Just ys

    isJust . checkedTest ?> \x -> do
        -- FIXME: Could optimise this test by calling rule directly and returning FileA Eq Eq Eq
        --        But only saves noticable time on uncommon Change modes
        _ :: FilesA <- apply1 $ FilesQ $ map (FileQ . packU_ . filepathNormalise . unpackU_ . packU) $ fromJust $ test x
        return ()

    rule $ \(FilesQ xs_) -> let xs@(x:_) = map (unpackU . fromFileQ) xs_ in
        case checkedTest x of
            Just ys | ys == xs -> Just $ \vo -> do
                liftIO $ mapM_ (createDirectoryIfMissing True) $ nubOrd $ map takeDirectory xs
                act xs
                getFileTimes "&?>" xs_ vo
            Just ys -> error $ "Error, &?> is incompatible with " ++ show xs ++ " vs " ++ show ys
            Nothing -> Nothing
            -}
