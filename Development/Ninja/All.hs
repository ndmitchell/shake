{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Env
import Development.Ninja.Type
import Development.Ninja.Parse
import Development.Shake hiding (Rule)
import Development.Shake.ByteString
import Development.Shake.File
import Development.Shake.Timing
import qualified Data.ByteString.Char8 as BS

import System.Directory
import qualified Data.HashMap.Strict as Map
import Control.Arrow
import Control.Monad
import Data.List
import Data.Char


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    addTiming "Ninja parse"
    ninja@Ninja{..} <- parse file
    return $ do
        phonys <- return $ Map.fromList phonys
        singles <- return $ Map.fromList $ map (first norm) singles
        multiples <- return $ Map.fromList [(x,(xs,b)) | (xs,b) <- map (first $ map norm) multiples, x <- xs]
        rules <- return $ Map.fromList rules
        pools <- fmap Map.fromList $ forM pools $ \(name,depth) ->
            fmap ((,) name) $ newResource (BS.unpack name) depth

        want $ map (BS.unpack . normalise) $ concatMap (resolvePhony phonys) $
            if not $ null args then map BS.pack args
            else if not $ null defaults then defaults
            else Map.keys singles ++ Map.keys multiples

        (\x -> fmap (map BS.unpack . fst) $ Map.lookup (BS.pack x) multiples) ?>> \out -> let out2 = map BS.pack out in
            build phonys rules pools out2 $ snd $ multiples Map.! head out2

        (flip Map.member singles . BS.pack) ?> \out -> let out2 = BS.pack out in
            build phonys rules pools [out2] $ singles Map.! out2


-- Normalise the LHS of build rules, so that normalised RHS still match
norm :: Str -> Str
norm = normalise


resolvePhony :: Map.HashMap Str [Str] -> Str -> [Str]
resolvePhony mp = f $ Left 100
    where
        f (Left 0) x = f (Right []) x
        f (Right xs) x | x `elem` xs = error $ "Recursive phony involving " ++ BS.unpack x
        f a x = case Map.lookup x mp of
            Nothing -> [x]
            Just xs -> concatMap (f $ either (Left . subtract 1) (Right . (x:)) a) xs


quote :: Str -> Str
quote x | BS.any isSpace x = let q = BS.singleton '\"' in BS.concat [q,x,q]
        | otherwise = x


build :: Map.HashMap Str [Str] -> Map.HashMap Str Rule -> Map.HashMap Str Resource -> [Str] -> Build -> Action ()
build phonys rules pools out Build{..} = do
    needBS $ map normalise $ concatMap (resolvePhony phonys) $ depsNormal ++ depsImplicit ++ depsOrderOnly
    case Map.lookup ruleName rules of
        Nothing -> error $ "Ninja rule named " ++ BS.unpack ruleName ++ " is missing, required to build " ++ BS.unpack (BS.unwords out)
        Just Rule{..} -> do
            env <- liftIO $ scopeEnv env
            liftIO $ do
                -- the order of adding new environment variables matters
                addEnv env (BS.pack "out") (BS.unwords $ map quote out)
                addEnv env (BS.pack "in") (BS.unwords $ map quote depsNormal)
                addEnv env (BS.pack "in_newline") (BS.unlines depsNormal)
                addBinds env buildBind
                addBinds env ruleBind

            applyRspfile env $ do
                commandline <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "command"
                depfile <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "depfile"
                deps <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "deps"
                description <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "description"
                pool <- liftIO $ askVar env $ BS.pack "pool"

                let withPool act = case Map.lookup pool pools of
                        _ | BS.null pool -> act
                        Nothing -> error $ "Ninja pool named " ++ BS.unpack pool ++ " not found, required to build " ++ BS.unpack (BS.unwords out)
                        Just r -> withResource r 1 act

                when (description /= "") $ putNormal description
                if deps == "msvc" then do
                    Stdout stdout <- withPool $ command [Shell, EchoStdout True] commandline []
                    needBS $ map (normalise . BS.pack) $ parseShowIncludes stdout
                 else
                    withPool $ command_ [Shell] commandline []
                when (depfile /= "") $ do
                    when (deps /= "gcc") $ need [depfile]
                    depsrc <- liftIO $ BS.readFile depfile
                    needBS $ concatMap snd $ parseMakefile depsrc
                    when (deps == "gcc") $ liftIO $ removeFile depfile


applyRspfile :: Env Str Str -> Action a -> Action a
applyRspfile env act = do
    rspfile <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "rspfile"
    rspfile_content <- liftIO $ askVar env $ BS.pack "rspfile_content"
    if rspfile == "" then
        act
     else do
        liftIO $ BS.writeFile rspfile rspfile_content
        res <- act
        liftIO $ removeFile rspfile
        return res


parseShowIncludes :: String -> [FilePath]
parseShowIncludes out = [y | x <- lines out, Just x <- [stripPrefix "Note: including file:" x]
                           , let y = dropWhile isSpace x, not $ isSystemInclude y]


-- Dodgy, but ported over from the original Ninja
isSystemInclude :: String -> Bool
isSystemInclude x = "program files" `isInfixOf` lx || "microsoft visual studio" `isInfixOf` lx
    where lx = map toLower x
