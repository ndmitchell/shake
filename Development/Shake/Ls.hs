
module Development.Shake.Ls(lsRules, ls) where

import Development.Shake.Core
import Development.Shake.Derived
import System.Directory
import Control.Monad
import Data.List


newtype Ls = Ls FilePath

lsRules :: Rules ()
lsRules = rule1 $ \(Ls x) -> Just $ io $ listFiles x


ls :: FilePath -> Make [FilePath]
ls x = need1 $ Ls x


listFiles :: FilePath -> IO [FilePath]
listFiles = undefined