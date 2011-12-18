
-- | Main module for defining Shake build systems. You may also want to include
--   "Development.Shake.FilePath".
module Development.Shake(
    shake,
    -- * Core of Shake
    module Development.Shake.Core,
    -- * Utility functions
    module Development.Shake.Derived,
    -- * File rules
    module Development.Shake.File,
    -- * Directory rules
    module Development.Shake.Directory
    ) where

import Development.Shake.Core hiding (run)
import Development.Shake.Derived
import Development.Shake.File hiding (defaultRuleFile)
import Development.Shake.Directory hiding (defaultRuleDirectory)

import qualified Development.Shake.Core as X
import qualified Development.Shake.File as X
import qualified Development.Shake.Directory as X

-- | Main entry point for running Shake build systems.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    X.run opts $ do
        r
        X.defaultRuleFile
        X.defaultRuleDirectory
    return ()
