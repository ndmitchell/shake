-- | Fake cabal module for local building

module Paths_shake where

import Data.Version

getDataFileName :: FilePath -> IO FilePath
getDataFileName x = return $ "./" ++ x

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
