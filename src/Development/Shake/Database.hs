{-# LANGUAGE RecordWildCards #-}

-- |
module Development.Shake.Database(
    ShakeDatabase,
    shakeOpenDatabase,
    shakeWithDatabase,
    shakeOneShotDatabase,
    shakeRunDatabase,
    shakeLiveFilesDatabase,
    shakeRunAfter
    ) where

import Control.Concurrent.Extra
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import General.Cleanup
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Rules.Default
import Prelude

data UseState
    = Closed
    | Using String
    | Open {openOneShot :: Bool, openRequiresReset :: Bool}


data ShakeDatabase = ShakeDatabase (Var UseState) RunState

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = do
    (cleanup, clean) <- newCleanup
    use <- newVar $ Open False False
    let alloc =
            withOpen use "shakeOpenDatabase" id $ \_ ->
                ShakeDatabase use <$> open cleanup opts (rules >> defaultRules)
    let free = do
            modifyVar_ use $ \x -> case x of
                    Using s -> throwM $ errorStructured "Error when calling shakeOpenDatabase close function, currently running" [("Existing call", Just s)] ""
                    _ -> return Closed
            clean
    return (alloc, free)

withOpen :: Var UseState -> String -> (UseState -> UseState) -> (UseState -> IO a) -> IO a
withOpen var name final act = mask $ \restore -> do
    o <- modifyVar var $ \x -> case x of
        Using s -> throwM $ errorStructured ("Error when calling " ++ name ++ ", currently running") [("Existing call", Just s)] ""
        Closed -> throwM $ errorStructured ("Error when calling " ++ name ++ ", already closed") [] ""
        o@Open{} -> return (Using name, o)
    let clean = writeVar var $ final o
    res <- restore (act o) `onException` clean
    clean
    return res

shakeOneShotDatabase :: ShakeDatabase -> IO ()
shakeOneShotDatabase (ShakeDatabase use db) =
    withOpen use "shakeOneShotDatabase" (\o -> o{openOneShot=True}) $ \_ -> return ()

shakeWithDatabase :: ShakeOptions -> Rules () -> (ShakeDatabase -> IO a) -> IO a
shakeWithDatabase opts rules act = do
    (db, clean) <- shakeOpenDatabase opts rules
    (act =<< db) `finally` clean

shakeLiveFilesDatabase :: ShakeDatabase -> IO [FilePath]
shakeLiveFilesDatabase (ShakeDatabase use s) =
    withOpen use "shakeLiveFilesDatabase" id $ \_ ->
        liveFilesState s

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase (ShakeDatabase use s) as =
    withOpen use "shakeRunDatabase" (\o -> o{openRequiresReset=True}) $ \Open{..} -> do
        when openRequiresReset $ do
            when openOneShot $
                throwM $ errorStructured "Error when calling shakeRunDatabase twice, after calling shakeOneShotDatabase" [] ""
            reset s
        (refs, as) <- fmap unzip $ forM as $ \a -> do
            ref <- newIORef Nothing
            return (ref, liftIO . writeIORef ref . Just =<< a)
        after <- run s openOneShot $ map void as
        results <- mapM readIORef refs
        case sequence results of
            Just result -> return (result, after)
            Nothing -> throwM $ errorInternal "Expected all results were written, but some where not"
