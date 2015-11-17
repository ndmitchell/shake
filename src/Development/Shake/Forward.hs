{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types, ScopedTypeVariables, MultiParamTypeClasses #-}

module Development.Shake.Forward(
    shakeForward, shakeArgsForward,
    forwardOptions, forwardRule,
    forP, parallel, par, later,
    cache, cacheAction
    ) where

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Command
import Development.Shake.Classes
import Data.IORef
import System.IO.Unsafe
import qualified Data.HashMap.Strict as Map
import Control.Monad

{-# NOINLINE forwards #-}
forwards :: IORef (Map.HashMap ForwardQ (Action ()))
forwards = unsafePerformIO $ newIORef Map.empty

newtype ForwardQ = ForwardQ Int
    deriving (Hashable,Typeable,Eq,NFData,Binary,Show)

newtype ForwardA = ForwardA ()
    deriving (Hashable,Typeable,Eq,NFData,Binary,Show)

instance Rule ForwardQ ForwardA where
    storedValue _ _ = return $ Just $ ForwardA ()

shakeForward :: ShakeOptions -> Action () -> IO ()
shakeForward opts act = shake (forwardOptions opts) (forwardRule act)

shakeArgsForward :: ShakeOptions -> Action () -> IO ()
shakeArgsForward opts act = shakeArgs (forwardOptions opts) (forwardRule act)

forwardRule :: Action () -> Rules ()
forwardRule act = do
    rule $ \k -> Just $ do
        res <- liftIO $ atomicModifyIORef forwards $ \mp -> (Map.delete k mp, Map.lookup k mp)
        case res of
            Nothing -> error "Failed to find action name"
            Just act -> act
        return $ ForwardA ()
    action act

forwardOptions :: ShakeOptions -> ShakeOptions
forwardOptions opts = opts{shakeCommandOptions=[AutoDeps]}


forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = parallel $ map f xs

parallel :: [Action a] -> Action [a]
parallel xs = do xs <- mapM later xs; sequence xs

par :: Action a -> Action b -> Action (a,b)
par a b = do a <- later a; b <- later b; liftM2 (,) a b


-- register an action, start it now, and demand the result when I force it.
-- will be implicitly demanded at the end if not before that.
later :: Action a -> Action (Action a)
later = return


cacheAction :: String -> Action () -> Action ()
cacheAction name action = do
    let key = ForwardQ $ hash name
    liftIO $ atomicModifyIORef forwards $ \mp -> (Map.insert key action mp, ())
    _ :: [ForwardA] <- apply [key]
    liftIO $ atomicModifyIORef forwards $ \mp -> (Map.delete key mp, ())

cache :: (forall r . CmdArguments r => r) -> Action ()
cache cmd = do
    let args :: [Either CmdOption String] = cmd
    cacheAction ("cmd " ++ show args) cmd
