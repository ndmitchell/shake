
-- | The information from Paths_shake cleaned up
module Development.Shake.Internal.Paths(
    getDataFileName, shakeVersionString
    ) where

import Paths_shake
import Data.Version


shakeVersionString :: String
shakeVersionString = showVersion version
