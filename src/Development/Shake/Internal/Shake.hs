
-- | The main entry point that calls all the default rules
module Development.Shake.Internal.Shake(shake) where

import Development.Shake.Internal.Options
import General.Timing
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules

import Development.Shake.Internal.Rules.Directory
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Rules.Files
import Development.Shake.Internal.Rules.Rerun


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build. The function will throw
--   an exception if the build fails.
--
--   To use command line flags to modify 'ShakeOptions' see 'Development.Shake.shakeArgs'.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    addTiming "Function shake"
    run opts $ do
        r
        defaultRuleFile
        defaultRuleFiles
        defaultRuleDirectory
        defaultRuleRerun
    return ()
