{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Env
import Development.Ninja.Type
import Development.Ninja.Parse
import Development.Shake hiding (addEnv)
import Development.Shake.ByteString
import Development.Shake.Errors
import Development.Shake.Rules.File
import Development.Shake.Rules.OrderOnly
import General.Timing
import qualified Data.ByteString as BS8
import qualified Data.ByteString.Char8 as BS

import System.Directory
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Char


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    addTiming "Ninja parse"
    ninja@Ninja{..} <- parse file =<< newEnv
    return $ do
        needDeps <- return $ needDeps ninja -- partial application
        phonys <- return $ Map.fromList phonys
        singles <- return $ Map.fromList $ map (first normalise) singles
        multiples <- return $ Map.fromList [(x,(xs,b)) | (xs,b) <- map (first $ map normalise) multiples, x <- xs]
        rules <- return $ Map.fromList rules
        pools <- fmap Map.fromList $ forM pools $ \(name,depth) ->
            fmap ((,) name) $ newResource (BS.unpack name) depth

        action $ needBS $ map normalise $ concatMap (resolvePhony phonys) $
            if not $ null args then map BS.pack args
            else if not $ null defaults then defaults
            else Map.keys singles ++ Map.keys multiples

        (\x -> fmap (map BS.unpack . fst) $ Map.lookup (BS.pack x) multiples) ?>> \out -> let out2 = map BS.pack out in
            build needDeps phonys rules pools out2 $ snd $ multiples Map.! head out2

        (flip Map.member singles . BS.pack) ?> \out -> let out2 = BS.pack out in
            build needDeps phonys rules pools [out2] $ singles Map.! out2


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


build :: (Build -> [Str] -> Action ()) -> Map.HashMap Str [Str] -> Map.HashMap Str Rule -> Map.HashMap Str Resource -> [Str] -> Build -> Action ()
build needDeps phonys rules pools out build@Build{..} = do
    needBS $ map normalise $ concatMap (resolvePhony phonys) $ depsNormal ++ depsImplicit
    orderOnlyBS $ map normalise $ concatMap (resolvePhony phonys) depsOrderOnly
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
                    Stdout stdout <- withPool $ command [Shell] commandline []
                    needDeps build $ map normalise $ parseShowIncludes $ BS.pack stdout
                 else
                    withPool $ command_ [Shell] commandline []
                when (depfile /= "") $ do
                    when (deps /= "gcc") $ need [depfile]
                    depsrc <- liftIO $ BS.readFile depfile
                    needDeps build $ concatMap snd $ parseMakefile depsrc
                    when (deps == "gcc") $ liftIO $ removeFile depfile


needDeps :: Ninja -> Build -> [Str] -> Action ()
needDeps Ninja{..} = \build xs -> do -- eta reduced so 'builds' is shared
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then needBS xs else do
        neededBS xs
        -- now try and statically validate needed will never fail
        -- first find which dependencies are generated files
        xs <- return $ filter (`Map.member` builds) xs
        -- now try and find them as dependencies
        let bad = xs `difference` allDependencies build
        case bad of
            [] -> return ()
            x:_ -> errorStructured
                "Lint checking error - file in deps is generated and not a pre-dependency"
                [("File", Just $ BS.unpack x)]
                ""
    where
        builds :: Map.HashMap FileStr Build
        builds = Map.fromList $ singles ++ [(x,y) | (xs,y) <- multiples, x <- xs]

        -- do list difference, assuming a small initial set, most of which occurs early in the list
        difference :: [Str] -> [Str] -> [Str]
        difference [] ys = []
        difference xs ys = f (Set.fromList xs) ys
            where
                f xs [] = Set.toList xs
                f xs (y:ys) | y `Set.member` xs = if Set.null xs2 then [] else f xs2 ys
                    where xs2 = Set.delete y xs
                f xs (y:ys) = f xs ys

        -- find all dependencies of a rule, no duplicates, with all dependencies of this rule listed first
        allDependencies :: Build -> [FileStr]
        allDependencies rule = f Set.empty [] [rule]
            where
                f seen [] [] = []
                f seen [] (x:xs) = f seen (map normalise $ depsNormal x ++ depsImplicit x ++ depsOrderOnly x) xs
                f seen (x:xs) rest | x `Set.member` seen = f seen xs rest
                                   | otherwise = x : f (Set.insert x seen) xs (maybeToList (Map.lookup x builds) ++ rest)


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


parseShowIncludes :: Str -> [FileStr]
parseShowIncludes out = [y | x <- BS.lines out, bsNote `BS.isPrefixOf` x
                           , let y = BS.dropWhile isSpace $ BS.drop (BS.length bsNote) x
                           , not $ isSystemInclude y]

-- Dodgy, but ported over from the original Ninja
isSystemInclude :: FileStr -> Bool
isSystemInclude x = bsProgFiles `BS.isInfixOf` tx || bsVisStudio `BS.isInfixOf` tx
    where tx = BS8.map (\c -> if c >= 97 then c - 32 else c) x
               -- optimised toUpper that only cares about letters and spaces

bsNote = BS.pack "Note: including file:"
bsProgFiles = BS.pack "PROGRAM FILES"
bsVisStudio = BS.pack "MICROSOFT VISUAL STUDIO"
