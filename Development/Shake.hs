
-- | Main module for defining Shake build systems. You may also want to include
--   "Development.Shake.FilePath", for manipulating file paths. As a simple example,
--   let us build a @result.tar@ file from the contents of @result.txt@:
--
-- @
--import "Development.Shake"
--import "Development.Shake.FilePath"
--
--main = 'shake' 'shakeOptions' $ do
--    'want' [\"result.tar\"]
--    \"*.tar\" '*>' \\out -> do
--        contents <- 'readFileLines' $ replaceExtension out \"txt\"
--        'need' contents
--        'system'' \"tar\" $ [\"-cf\",out] ++ contents
-- @
--
--   For the background theory behind a previous version of Shake the online video:
--   <http://vimeo.com/15465133>.
module Development.Shake(
    shake,
    -- * Core of Shake
    ShakeOptions(..), shakeOptions,
    Rule(..), Rules, defaultRule, rule, action,
    Action, apply, apply1, traced,
    Verbosity(..), getVerbosity, putLoud, putNormal, putQuiet,
    Observed(..),
    liftIO,
    -- * Utility functions
    module Development.Shake.Derived,
    -- * File rules
    need, want, (*>), (**>), (?>),
    module Development.Shake.Files,
    FilePattern, (?==),
    -- * Directory rules
    doesFileExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    -- * Oracle rules
    module Development.Shake.Oracle
    ) where

-- I would love to use module export in the above export list, but alas Haddock
-- then shows all the things that are hidden in the docs, which is terrible.

import Control.Monad.IO.Class
import Development.Shake.Core hiding (run)
import Development.Shake.Derived
import Development.Shake.File hiding (defaultRuleFile)
import Development.Shake.FilePattern(FilePattern, (?==))
import Development.Shake.Files
import Development.Shake.Directory hiding (defaultRuleDirectory)
import Development.Shake.Oracle

import qualified Development.Shake.Core as X
import qualified Development.Shake.File as X
import qualified Development.Shake.Directory as X

-- | Main entry point for running Shake build systems. For an example see "Development.Shake".
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    X.run opts $ do
        r
        X.defaultRuleFile
        X.defaultRuleDirectory
    return ()
