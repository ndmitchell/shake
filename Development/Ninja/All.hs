{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Parse
import Development.Ninja.Eval
import Development.Shake
import Development.Shake.Command

import System.IO
import Control.Exception
import System.Directory
import qualified Data.HashMap.Strict as Map
import Control.Monad
import Data.List
import Control.Arrow
import Data.Char


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    Ninja{..} <- fmap eval $ parse file
    return $ do
        pools <- fmap Map.fromList $ forM (Map.toList pools) $ \(name,depth) ->
            fmap ((,) name) $ newResource name depth
        want defaults
        forM_ phonys $ \(name,file) -> phony name $ need [file]

        flip Map.member singles ?> \file -> do
            let Builder{..} = singles Map.! file
            let (deps, impdeps) = second (drop 1) $ break (== "|") dependencies
            need $ deps ++ impdeps
            case Map.lookup ruleName rules of
                Nothing -> error $ "Ninja rule named " ++ ruleName ++ " is missing, required to build " ++ show file
                Just bind2 -> do
                    let env = addEnv "in_newline" (unlines deps) $
                              addEnv "in" (unwords deps) $
                              addEnv "out" file defines
                    let f env (name,val) = addEnv name (askEnv env val) env
                    let fs xs env = foldl f env xs
                    env <- return $ fs bind2 $ fs bindings env

                    let rspfile_content = askEnv env $ var "rspfile_content"
                    applyRspfile env rspfile_content $ \env -> do
                        let commandline = askEnv env $ var "command"
                        let depfile = askEnv env $ var "depfile"
                        let deps = askEnv env $ var "deps"
                        let description = askEnv env $ var "description"

                        when (description /= "") $ putNormal description
                        liftIO $ print commandline
                        if deps == "" then do
                            command_ [Shell] commandline []
                         else do
                            Stdout out <- command [Shell] commandline []
                            need $ applyDeps deps out
                        when (depfile /= "") $ do
                            depfile <- liftIO $ readFile depfile
                            need $ applyDepfile depfile



applyRspfile :: Env -> String -> (Env -> Action a) -> Action a
applyRspfile env "" act = act env
applyRspfile env contents act = do
    tmp <- liftIO getTemporaryDirectory
    (path, handle) <- liftIO $ openTempFile tmp "shake.tmp"
    let cleanup = catch (removeFile path) (\(_ :: SomeException) -> return ())
    flip actionFinally cleanup $ do
        liftIO $ hPutStr handle contents `finally` hClose handle
        act $ addEnv "rspfile" path env


applyDeps :: String -> String -> [FilePath]
applyDeps "gcc" out = error $ "applyDeps gcc: " ++ show out
applyDeps "msvc" out = [y | x <- lines out, Just x <- [stripPrefix "Note: including file:" x]
                          , let y = dropWhile isSpace x, not $ isSystemInclude y]
applyDeps name _ = error $ "Don't understand deps = " ++ name


applyDepfile :: String -> [FilePath]
applyDepfile src = error $ "applyDepfile: " ++ show src


-- Dodgy, but ported over from the original Ninja
isSystemInclude :: String -> Bool
isSystemInclude x = "program files" `isInfixOf` lx || "microsoft visual studio" `isInfixOf` lx
    where lx = map toLower x
