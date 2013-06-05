{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Type
import Development.Ninja.Parse
import Development.Shake hiding (Rule)
import Development.Shake.Command
import qualified Data.ByteString.Char8 as BS

import System.Directory
import qualified Data.HashMap.Strict as Map
import Control.Monad
import Data.List
import Data.Char
import qualified System.FilePath as FP(normalise)


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    ninja@Ninja{..} <- parse file
    return $ do
        want $ if null args then map BS.unpack defaults else args
        forM_ phonys $ \(name,files) -> phony (BS.unpack name) $ need $ map BS.unpack files

        singles <- return $ Map.fromList singles
        multiples <- return $ Map.fromList [(x,(xs,b)) | (xs,b) <- multiples, x <- xs]
        rules <- return $ Map.fromList rules
        pools <- fmap Map.fromList $ forM pools $ \(name,depth) ->
            fmap ((,) name) $ newResource (BS.unpack name) depth

        (\x -> fmap (map BS.unpack . fst) $ Map.lookup (BS.pack x) multiples) ?>> \out -> let out2 = map BS.pack out in
            build defines rules pools out2 $ snd $ multiples Map.! head out2

        (flip Map.member singles . BS.pack) ?> \out -> let out2 = BS.pack out in
            build defines rules pools [out2] $ singles Map.! out2


build :: Env -> Map.HashMap Str Rule -> Map.HashMap Str Resource -> [Str] -> Build -> Action ()
build env rules pools out Build{..} = do
    need $ map BS.unpack $ depsNormal ++ depsImplicit ++ depsOrderOnly
    case Map.lookup ruleName rules of
        Nothing -> error $ "Ninja rule named " ++ BS.unpack ruleName ++ " is missing, required to build " ++ BS.unpack (BS.unwords out)
        Just Rule{..} -> do
            env <- return $
                addBinds ruleBind $ addBinds buildBind $
                addEnv (BS.pack "in_newline") (BS.unlines depsNormal) $
                addEnv (BS.pack "in") (BS.unwords depsNormal) $
                addEnv (BS.pack "out") (BS.unwords out) env

            applyRspfile env $ do
                let commandline = BS.unpack $ askVar env $ BS.pack "command"
                let depfile = BS.unpack $ askVar env $ BS.pack "depfile"
                let deps = BS.unpack $ askVar env $ BS.pack "deps"
                let description = BS.unpack $ askVar env $ BS.pack "description"
                let pool = askVar env $ BS.pack "pool"

                let withPool act = case Map.lookup pool pools of
                        _ | BS.null pool -> act
                        Nothing -> error $ "Ninja pool named " ++ BS.unpack pool ++ " not found, required to build " ++ BS.unpack (BS.unwords out)
                        Just r -> withResource r 1 act

                when (description /= "") $ putNormal description
                if deps == "msvc" then do
                    Stdout stdout <- withPool $ command [Shell, EchoStdout True] commandline []
                    need $ parseShowIncludes stdout
                 else
                    withPool $ command_ [Shell] commandline []
                when (depfile /= "") $ do
                    when (deps /= "gcc") $ need [depfile]
                    depsrc <- liftIO $ readFile depfile
                    need $ concatMap snd $ parseMakefile depsrc
                    when (deps == "gcc") $ liftIO $ removeFile depfile


applyRspfile :: Env -> Action a -> Action a
applyRspfile env act
    | rspfile == "" = act
    | otherwise = do
        liftIO $ BS.writeFile rspfile rspfile_content
        res <- act
        liftIO $ removeFile rspfile
        return res
    where
        rspfile = BS.unpack $ askVar env $ BS.pack "rspfile"
        rspfile_content = askVar env $ BS.pack "rspfile_content"


parseShowIncludes :: String -> [FilePath]
parseShowIncludes out = [y | x <- lines out, Just x <- [stripPrefix "Note: including file:" x]
                           , let y = dropWhile isSpace x, not $ isSystemInclude y]


-- Dodgy, but ported over from the original Ninja
isSystemInclude :: String -> Bool
isSystemInclude x = "program files" `isInfixOf` lx || "microsoft visual studio" `isInfixOf` lx
    where lx = map toLower x


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = concatMap f . join . lines
    where
        join (x1:x2:xs) | "\\" `isSuffixOf` x1 = join $ (init x1 ++ x2) : xs
        join (x:xs) = x : join xs
        join [] = []

        f x = [(FP.normalise a, map FP.normalise $ words $ drop 1 b) | a <- words a]
            where (a,b) = break (== ':') $ takeWhile (/= '#') x
