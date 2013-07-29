{-# LANGUAGE CPP #-}

-- | This module is used for defining Shake build systems. As a simple example of a Shake build system,
--   let us build the file @result.tar@ from the files listed by @result.txt@:
--
-- @
--import "Development.Shake"
--import "Development.Shake.FilePath"
--
--main = 'shakeArgs' 'shakeOptions' $ do
--    'want' [\"result.tar\"]
--    \"*.tar\" '*>' \\out -> do
--        contents \<- 'readFileLines' $ out 'Development.Shake.FilePath.-<.>' \"txt\"
--        'need' contents
--        'cmd' \"tar -cf\" [out] contents
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
--   with '*>' to produce the results. Before calling 'cmd' you should ensure that any files the command
--   requires are demanded with calls to 'need'. We offer the following advice to Shake users:
--
-- * If @ghc --make@ or @cabal@ is capable of building your project, use that instead. Custom build systems are
--   necessary for many complex projects, but many projects are not complex.
--
-- * The 'shakeArgs' function automatically handles command line arguments. To define non-file targets use 'phony'.
--
-- * Put all result files in a distinguished directory, for example @_make@. You can implement a @clean@
--   command by removing that directory, using @'removeFilesAfter' \"_make\" [\"\/\/\*\"]@.
--
-- * To obtain parallel builds set 'shakeThreads' to a number greater than 1. You may also need to
--   compile with @-threaded@.
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
--   The theory behind Shake is described in an ICFP 2012 paper, Shake Before Building -- Replacing Make with Haskell
--   <http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf>. The associated talk
--   forms a short overview of Shake <http://www.youtube.com/watch?v=xYCPpXVlqFM>.
--
--   /Acknowledgements/: Thanks to Austin Seipp for properly integrating the profiling code.
module Development.Shake(
    -- * Core
    shake,
    shakeOptions,
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue,
#endif
    Rule(..), Rules, defaultRule, rule, action, withoutActions,
    Action, apply, apply1, traced,
    liftIO, actionOnException, actionFinally,
    ShakeException(..),
    -- * Configuration
    ShakeOptions(..), Assume(..),
    -- ** Command line
    shakeArgs, shakeArgsWith, shakeOptDescrs,
    -- ** Progress reporting
    Progress(..), progressSimple, progressDisplay, progressTitlebar,
    -- ** Verbosity
    Verbosity(..), getVerbosity, putLoud, putNormal, putQuiet, quietly,
    -- * Running commands
    command, command_, cmd,
    Stdout(..), Stderr(..), Exit(..),
    CmdResult, CmdOption(..),
    -- * Utility functions
    module Development.Shake.Derived,
    removeFiles, removeFilesAfter,
    -- * File rules
    need, want, (*>), (**>), (?>), phony, (~>),
    module Development.Shake.Files,
    FilePattern, (?==),
    -- * Directory rules
    doesFileExist, doesDirectoryExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    -- * Environment rules
    getEnv,
    -- * Oracle rules
    addOracle, askOracle, askOracleWith,
    -- * Special rules
    alwaysRerun,
    -- * Resources
    Resource, newResource, newResourceIO, withResource, withResources,
    newThrottle, newThrottleIO,
    unsafeExtraThread,
    -- * Cached file contents
    newCache, newCacheIO
    ) where

-- I would love to use module export in the above export list, but alas Haddock
-- then shows all the things that are hidden in the docs, which is terrible.

import Control.Monad.IO.Class
import Development.Shake.Types
import Development.Shake.Core
import Development.Shake.Derived
import Development.Shake.Errors
import Development.Shake.Progress
import Development.Shake.Args
import Development.Shake.Shake

import Development.Shake.Command
import Development.Shake.Directory
import Development.Shake.File
import Development.Shake.FilePattern
import Development.Shake.Files
import Development.Shake.Oracle
import Development.Shake.Rerun
