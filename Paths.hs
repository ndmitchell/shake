-- | Fake cabal module for local building

module Paths_shake where

getDataFileName :: FilePath -> IO FilePath
getDataFileName x = return $ "./" ++ x
