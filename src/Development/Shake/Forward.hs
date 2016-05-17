{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, Rank2Types, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

-- | A module for producing forward-defined build systems, in contrast to standard backwards-defined
--   build systems such as shake. Based around ideas from <https://code.google.com/p/fabricate/ fabricate>.
--   As an example:
--
-- @
-- import "Development.Shake"
-- import "Development.Shake.Forward"
-- import "Development.Shake.FilePath"
--
-- main = 'shakeArgsForward' 'shakeOptions' $ do
--     contents <- 'readFileLines' \"result.txt\"
--     'cache' $ 'cmd' \"tar -cf result.tar\" contents
-- @
--
--   Compared to backward-defined build systems (such as normal Shake), forward-defined build
--   systems tend to be simpler for simple systems (less boilerplate, more direct style), but more
--   complex for larger build systems (requires explicit parallelism, explicit sharing of build products,
--   no automatic command line targets). As a general approach for writing forward-defined systems:
--
-- * Figure out the sequence of system commands that will build your project.
--
-- * Write a simple 'Action' that builds your project.
--
-- * Insert 'cache' in front of most system commands.
--
-- * Replace most loops with 'forP', where they can be executed in parallel.
--
-- * Where Haskell performs real computation, if zero-build performance is insufficient, use 'cacheAction'.
--
--   All forward-defined systems use 'AutoDeps', which requires @fsatrace@ to be on the @$PATH@.
--   You can obtain @fsatrace@ from <https://github.com/jacereda/fsatrace>.
module Development.Shake.Forward(
    shakeForward, shakeArgsForward,
    forwardOptions, forwardRule,
    cache, cacheAction
    ) where

import Development.Shake
import Development.Shake.Core
import Development.Shake.Core2
import Development.Shake.Monad
import Development.Shake.Command
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Value

import Data.Binary
import Data.Binary.Put
import Data.Either
import Data.List.Extra
import Control.Exception.Extra
import Numeric
import Data.IORef
import qualified Data.HashMap.Strict as Map

-{-# NOINLINE globalForwards #-}
globalForwards :: IORef (Map.HashMap Value (Action Value))
globalForwards = unsafePerformIO $ newIORef Map.empty

newtype ForwardQ = ForwardQ Value
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Given an 'Action', turn it into a 'Rules' structure which runs in forward mode.
forwardRule :: Action () -> Rules ()
forwardRule act = do
    liftIO $ registerWitness $ (undefined :: String)
    addBuiltinRule $ \(ForwardQ k) vo dep -> do
        res <- liftIO $ atomicModifyIORef globalForwards $ \mp -> (Map.delete k mp, Map.lookup k mp)
        v <- case res of
            Nothing -> liftIO . errorIO $ "Failed to find action: " ++ show k
            _ | dep, Just vo <- vo -> return vo
            Just act -> act
        return $ BuiltinResult v v (not dep) $ maybe False ((==) v) vo
    action act

-- | Cache an action. The action's key must be globally unique over all runs (i.e., change if the code changes).
--   The result is compared for equality using its 'Binary' serialization.
--
--   /If you use this function with a key type other than 'String', you must register it using 'registerWitness' before running Shake/
--
-- An example:
--
-- @
-- import "Development.Shake"
-- import "Development.Shake.Forward"
-- import "Development.Shake.Value"
--
-- main = do
--     'registerWitness' (undefined :: Integer)
--     'shakeArgsForward' 'shakeOptions' $ do
--         exists <- 'cacheAction' (0 :: Integer) $ 'doesFileExist' \"result.txt\"
--         'cacheAction' (1 :: Integer) $ writeFile' \"result2.txt\" (show exists)
-- @
cacheAction :: (Typeable a, Binary a, Binary b) => a -> Action b -> Action b
cacheAction name action = do
    ws <- liftIO currentWitness
    let key = runPut $ putKeyWith ws $ newKey name
        act = fmap encode action
    liftIO $ evaluate $ rnf key
    liftIO $ atomicModifyIORef globalForwards $ \mp -> (Map.insert key act mp, ())
    res <- fmap decode . apply1 $ ForwardQ key
    liftIO $ atomicModifyIORef globalForwards $ \mp -> (Map.delete key mp, ()) -- needed?
    return res

-- | Run a forward-defined build system.
shakeForward :: ShakeOptions -> Action () -> IO ()
shakeForward opts act = shake (forwardOptions opts) (forwardRule act)

-- | Run a forward-defined build system, interpretting command-line arguments.
shakeArgsForward :: ShakeOptions -> Action () -> IO ()
shakeArgsForward opts act = shakeArgs (forwardOptions opts) (forwardRule act)

-- | Given a 'ShakeOptions', set the options necessary to execute in forward mode.
forwardOptions :: ShakeOptions -> ShakeOptions
forwardOptions opts = opts{shakeCommandOptions=[AutoDeps]}

-- | Apply caching to an external command.
cache :: (forall r . CmdArguments r => r) -> Action ()
cache cmd = do
    let args :: [Either CmdOption String] = cmd
    let isDull ['-',x] = True; isDull _ = False
    let name = head $ filter (not . isDull) (drop 1 $ rights args) ++ ["unknown"]
    cacheAction ("command " ++ toStandard name ++ " #" ++ upper (showHex (abs $ hash $ show args) "")) cmd
