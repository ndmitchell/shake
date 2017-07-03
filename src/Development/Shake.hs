
-- | This module is used for defining Shake build systems. As a simple example of a Shake build system,
--   let us build the file @result.tar@ from the files listed by @result.txt@:
--
-- @
-- import "Development.Shake"
-- import "Development.Shake.FilePath"
--
-- main = 'shakeArgs' 'shakeOptions' $ do
--     'want' [\"result.tar\"]
--     \"*.tar\" '%>' \\out -> do
--         contents \<- 'readFileLines' $ out 'Development.Shake.FilePath.-<.>' \"txt\"
--         'need' contents
--         'cmd' \"tar -cf\" [out] contents
-- @
--
--   We start by importing the modules defining both Shake and routines for manipulating 'FilePath' values.
--   We define @main@ to call 'shake' with the default 'shakeOptions'. As the second argument to
--   'shake', we provide a set of rules. There are two common forms of rules, 'want' to specify target files,
--   and '%>' to define a rule which builds a 'FilePattern'. We use 'want' to require that after the build
--   completes the file @result.tar@ should be ready.
--
--   The @*.tar@ rule describes how to build files with the extension @.tar@, including @result.tar@.
--   We 'readFileLines' on @result.txt@, after changing the @.tar@ extension to @.txt@. We read each line
--   into the variable @contents@ -- being a list of the files that should go into @result.tar@. Next, we
--   depend ('need') all the files in @contents@. If any of these files change, the rule will be repeated.
--   Finally we call the @tar@ program. If either @result.txt@ changes, or any of the files listed by @result.txt@
--   change, then @result.tar@ will be rebuilt.
--
--   To find out more:
--
-- * The user manual contains a longer example and background information on how to use Shake
--   <http://www.shakebuild.com/manual>.
--
-- * The home page has links to additional information <http://www.shakebuild.com/>, including
--   a mailing list.
--
-- * The theory behind Shake is described in an ICFP 2012 paper,
--   <http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf Shake Before Building -- Replacing Make with Haskell>.
--   The <http://www.youtube.com/watch?v=xYCPpXVlqFM associated talk> forms a short overview of Shake .
module Development.Shake(
    -- * Writing a build system
    -- $writing

    -- * GHC build flags
    -- $flags

    -- * Core
    shake,
    shakeOptions,
    Rules, action, withoutActions, alternatives, priority,
    Action, traced,
    liftIO, actionOnException, actionFinally,
    ShakeException(..),
    -- * Configuration
    ShakeOptions(..), Rebuild(..), Lint(..), Change(..),
    getShakeOptions, getShakeOptionsRules, getHashedShakeVersion,
    getShakeExtra, addShakeExtra,
    -- ** Command line
    shakeArgs, shakeArgsWith, shakeOptDescrs,
    -- ** Progress reporting
    Progress(..), progressSimple, progressDisplay, progressTitlebar, progressProgram, getProgress,
    -- ** Verbosity
    Verbosity(..), getVerbosity, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    -- * Running commands
    command, command_, cmd, cmd_, unit,
    Stdout(..), Stderr(..), Stdouterr(..), Exit(..), Process(..), CmdTime(..), CmdLine(..),
    CmdResult, CmdString, CmdOption(..),
    addPath, addEnv,
    -- * Explicit parallelism
    parallel, forP, par,
    -- * Utility functions
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged,
    removeFiles, removeFilesAfter,
    withTempFile, withTempDir,
    -- * File rules
    need, want, (%>), (|%>), (?>), phony, (~>), phonys,
    (&%>), (&?>),
    orderOnly, orderOnlyAction,
    FilePattern, (?==), (<//>), filePattern,
    needed, trackRead, trackWrite, trackAllow,
    -- * Directory rules
    doesFileExist, doesDirectoryExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getDirectoryFilesIO,
    -- * Environment rules
    getEnv, getEnvWithDefault,
    -- * Oracle rules
    ShakeValue, RuleResult, addOracle, askOracle, askOracleWith,
    -- * Special rules
    alwaysRerun,
    -- * Resources
    Resource, newResource, newResourceIO, withResource, withResources,
    newThrottle, newThrottleIO,
    unsafeExtraThread,
    -- * Cache
    newCache, newCacheIO,
    -- * Deprecated
    (*>), (|*>), (&*>),
    (**>), (*>>), (?>>)
    ) where

import Prelude(Maybe, FilePath) -- Since GHC 7.10 duplicates *>

-- I would love to use module export in the above export list, but alas Haddock
-- then shows all the things that are hidden in the docs, which is terrible.
import Control.Monad.IO.Class
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Derived
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Progress
import Development.Shake.Internal.Args
import Development.Shake.Internal.Shake

import Development.Shake.Command
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.Rules.Directory
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Rules.Files
import Development.Shake.Internal.Rules.Oracle
import Development.Shake.Internal.Rules.OrderOnly
import Development.Shake.Internal.Rules.Rerun

-- $writing
--
--   When writing a Shake build system, start by defining what you 'want', then write rules
--   with '%>' to produce the results. Before calling 'cmd' you should ensure that any files the command
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
-- * To obtain parallel builds set 'shakeThreads' to a number greater than 1.
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

-- $flags
--
--   For large build systems the choice of GHC flags can have a significant impact. We recommend:
--
-- > ghc --make MyBuildSystem -threaded -rtsopts "-with-rtsopts=-I0 -qg qb"
--
--   * @-rtsopts@: Allow the setting of further GHC options at runtime.
--
--   * @-I0@: Disable idle garbage collection, to avoid frequent unnecessary garbage collection, see
--     <http://stackoverflow.com/questions/34588057/why-does-shake-recommend-disabling-idle-garbage-collection/ a full explanation>.
--
--   * With GHC 7.6 and before, omit @-threaded@: <http://ghc.haskell.org/trac/ghc/ticket/7646 GHC bug 7646>
--     can cause a race condition in build systems that write files then read them. Omitting @-threaded@ will
--     still allow your 'cmd' actions to run in parallel, so most build systems will still run in parallel.
--
--   * With GHC 7.8 and later you may add @-threaded@, and pass the options @-qg -qb@ to @-with-rtsopts@
--     to disable parallel garbage collection. Parallel garbage collection in Shake
--     programs typically goes slower than sequential garbage collection, while occupying many cores that
--     could be used for running system commands.


---------------------------------------------------------------------
-- DEPRECATED SINCE 0.13, MAY 2014

infix 1 **>, ?>>, *>>

-- | /Deprecated:/ Alias for '|%>'.
(**>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(**>) = (|%>)

-- | /Deprecated:/ Alias for '&?>'.
(?>>) :: (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
(?>>) = (&?>)

-- | /Deprecated:/ Alias for '&%>'.
(*>>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
(*>>) = (&%>)


---------------------------------------------------------------------
-- DEPRECATED SINCE 0.14, MAY 2014

infix 1 *>, |*>, &*>

-- | /Deprecated:/ Alias for '%>'. Note that @*>@ clashes with a Prelude operator in GHC 7.10.
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(*>) = (%>)

-- | /Deprecated:/ Alias for '|%>'.
(|*>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(|*>) = (|%>)

-- | /Deprecated:/ Alias for '&%>'.
(&*>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
(&*>) = (&%>)
