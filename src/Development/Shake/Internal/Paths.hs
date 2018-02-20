
-- | The information from Paths_shake cleaned up
module Development.Shake.Internal.Paths(
    getDataFileName, initDataDirectory,
    shakeVersionString
    ) where

import Paths_shake
import Control.Monad
import Data.Version


shakeVersionString :: String
shakeVersionString = showVersion version

initDataDirectory :: IO ()
-- my debug getDataFileName (in Paths) uses a cache of the Cwd
-- make sure we force the cache before changing directory
initDataDirectory = void $ getDataFileName ""
