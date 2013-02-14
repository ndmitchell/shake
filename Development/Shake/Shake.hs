
-- | The main entry point that calls all the default rules
module Development.Shake.Shake(shake) where

import Development.Shake.Types
import Development.Shake.Core

import Development.Shake.Directory
import Development.Shake.File
import Development.Shake.Rerun


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build. The function will throw
--   an exception if the build fails.
--
--   To use command line flags to modify 'ShakeOptions' see 'Development.Shake.shakeWithArgs'.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    run opts $ do
        r
        defaultRuleFile
        defaultRuleDirectory
        defaultRuleRerun
    return ()
