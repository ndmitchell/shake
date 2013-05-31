{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Parse
import Development.Ninja.Eval
import Development.Ninja.Type
import Development.Shake

import qualified Data.HashMap.Strict as Map
import Control.Monad


runNinja :: FilePath -> [String] -> IO (Rules ())
runNinja file args = do
    Ninja{..} <- fmap eval $ parse file
    return $ do
        pools <- fmap Map.fromList $ forM (Map.toList pools) $ \(name,depth) ->
            fmap ((,) name) $ newResource name depth
        want defaults
        forM_ phonys $ \(name,file) -> phony name $ need [file]

        flip Map.member builds ?> \file -> do
            let Build{..} = builds Map.! file
            need inputs
            case Map.lookup rule rules of
                Nothing -> error $ "Ninja rule named " ++ rule ++ " is missing, required to build " ++ show output
                Just bind2 -> do
                    error $ "Running: " ++ show bind2
