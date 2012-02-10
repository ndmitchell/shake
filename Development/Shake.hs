
-- | This module is used for defining Shake build systems. As a simple example of a Shake build system,
--   let us build the file @result.tar@ from the files listed by @result.txt@:
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
--   We start by importing the modules defining both Shake and routines for manipulating 'FilePath' values.
--   We define @main@ to call 'shake' with the default 'shakeOptions'. As the second argument to
--   'shake', we provide a set of rules. There are two common forms of rules, 'want' to specify target files,
--   and '*>' to define a rule which builds a 'FilePattern'. We use 'want' to require that after the build
--   completes the file @result.tar@ should be ready.
--
--   The @*.tar@ rule describes how to build files with the extension @.tar@, including @result.tar@.
--   We 'readFileLines' on @result.txt@, after changing the @.tar@ extension to @.txt@. We read each line
--   into the variable @contents@ -- being a list of the files that should go into @result.tar@. Next, we
--   depend ('need') all the files in @contents@. If any of these files change, the rule will be repeated.
--   Finally we call the @tar@ program. If either @result.txt@ changes, or any of the files listed by @result.txt@
--   change, then @result.tar@ will be rebuilt.
--
--   When writing a Shake build system, start by defining what you 'want', then write rules
--   with '*>' to produce the results. Before calling 'system'' you should ensure that any files the command
--   requires are demanded with calls to 'need'. We offer the following advice to Shake users:
--
-- * If @ghc --make@ or @cabal@ is capable of building your project, use that instead. Custom build systems are
--   necessary for many complex projects, but many projects are not complex.
--
-- * The CmdArgs package (<http://hackage.haskell.org/package/cmdargs/>) is well suited to providing
--   command line parsing for build systems, often using flags to set fields in 'shakeOptions'.
--
-- * Put all result files in a distinguished directory, for example @_make@. You can implement a @clean@
--   command by removing that directory, using 'removeDirectoryRecursive'.
--
-- * To obtain paralell builds set 'shakeThreads' to a number greater than 1. You may also need to
--   compile with @-threaded@.
--
-- * Often the 'want' commands will be determined by command line arguments, to mirror the behaviour of @make@
--   targets.
--
-- * Lots of compilers produce @.o@ files. To avoid overlapping rules, use @.c.o@ for C compilers,
--   @.hs.o@ for Haskell compilers etc.
--
-- * Do not be afraid to mix Shake rules, system commands and other Haskell libraries -- use each for what
--   it does best.
--
-- * The more accurate the dependencies are, the better. Use additional rules like 'doesFileExist' and
--   'getDirectoryFiles' to track information other than just the contents of files. For information in the environment
--   that you suspect will change regularly (perhaps @ghc@ version number), either write the information to
--   a file with 'alwaysRerun' and 'writeFileChanged', or use 'addOracle'.
--
--   The theory behind an old version of Shake is described in a video at <http://vimeo.com/15465133>.
module Development.Shake(
    shake,
    -- * Core of Shake
    ShakeOptions(..), shakeOptions,
    Rule(..), Rules, defaultRule, rule, action,
    Action, apply, apply1, traced,
    Verbosity(..), getVerbosity, putLoud, putNormal, putQuiet,
    liftIO,
    -- * Utility functions
    module Development.Shake.Derived,
    -- * File rules
    need, want, (*>), (**>), (?>),
    module Development.Shake.Files,
    FilePattern, (?==),
    -- * Directory rules
    doesFileExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    -- * Additional rules
    addOracle, askOracle,
    alwaysRerun
    ) where

-- I would love to use module export in the above export list, but alas Haddock
-- then shows all the things that are hidden in the docs, which is terrible.

import Control.Monad.IO.Class
import Development.Shake.Core
import Development.Shake.Derived

import Development.Shake.Directory
import Development.Shake.File
import Development.Shake.FilePattern
import Development.Shake.Files
import Development.Shake.Oracle
import Development.Shake.Rerun


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    run opts $ do
        r
        defaultRuleFile
        defaultRuleDirectory
        defaultRuleRerun
    return ()
