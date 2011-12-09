
module Development.Shake(
    shake,
    module Development.Shake.Core,
    module Development.Shake.Derived,
    module Development.Shake.File,
    module Development.Shake.Directory
    ) where

import Development.Shake.Core hiding (runShake)
import Development.Shake.Derived
import Development.Shake.File hiding (defaultRuleFile)
import Development.Shake.Directory hiding (defaultRuleDirectory)

import qualified Development.Shake.Core as X
import qualified Development.Shake.File as X
import qualified Development.Shake.Directory as X

shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    X.runShake opts $ do
        r
        X.defaultRuleFile
        X.defaultRuleDirectory
    return ()
