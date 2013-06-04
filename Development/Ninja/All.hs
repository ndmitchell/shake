{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Parse
import Development.Ninja.Eval
import Development.Shake
import Development.Shake.Command

import System.IO
import Control.Exception as E
import System.Directory
import qualified Data.HashMap.Strict as Map
import Control.Monad
import Data.List
import Data.Char
import qualified System.FilePath as FP(normalise)


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    ninja@Ninja{..} <- fmap eval $ parse file
    return $ do
        pools <- fmap Map.fromList $ forM (Map.toList pools) $ \(name,depth) ->
            fmap ((,) name) $ newResource name depth
        want $ if null args then defaults else args
        forM_ phonys $ \(name,files) -> phony name $ need files

        (\x -> fmap fst $ Map.lookup x multiples) ?>> \out ->
            build ninja pools out $ snd $ multiples Map.! head out

        flip Map.member singles ?> \out ->
            build ninja pools [out] $ singles Map.! out


build :: Ninja -> Map.HashMap String Resource -> [FilePath] -> Builder -> Action ()
build Ninja{..} resources out Builder{..} = do
    need $ deps ++ impDeps ++ ordDeps
    case Map.lookup ruleName rules of
        Nothing -> error $ "Ninja rule named " ++ ruleName ++ " is missing, required to build " ++ unwords out
        Just bind2 -> do
            let env = addEnv "in_newline" (unlines deps) $
                      addEnv "in" (unwords deps) $
                      addEnv "out" (unwords out) defines
            let f env (name,val) = addEnv name (askEnv env val) env
            let fs xs env = foldl f env xs
            env <- return $ fs bind2 $ fs bindings env

            let rspfile_content = askEnv env $ var "rspfile_content"
            applyRspfile env rspfile_content $ \env -> do
                let commandline = askEnv env $ var "command"
                let depfile = askEnv env $ var "depfile"
                let deps = askEnv env $ var "deps"
                let description = askEnv env $ var "description"
                let pool = askEnv env $ var "pool"

                let withPool act = case Map.lookup pool resources of
                        _ | pool == "" -> act
                        Nothing -> error $ "Ninja pool named " ++ pool ++ " not found, required to build " ++ unwords out
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



applyRspfile :: Env -> String -> (Env -> Action a) -> Action a
applyRspfile env "" act = act env
applyRspfile env contents act = do
    tmp <- liftIO getTemporaryDirectory
    (path, handle) <- liftIO $ openTempFile tmp "shake.tmp"
    let cleanup = E.catch (removeFile path) (\(_ :: SomeException) -> return ())
    flip actionFinally cleanup $ do
        liftIO $ hPutStr handle contents `finally` hClose handle
        act $ addEnv "rspfile" path env


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
