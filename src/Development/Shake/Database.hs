
-- | Module in development
module Development.Shake.Database(
    ShakeDatabase,
    shakeOpenDatabase,
    shakeWithDatabase,
    shakeRunDatabase,
    shakeLiveFilesDatabase,
    shakeRunAfters
    ) where

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


newtype ShakeDatabase = ShakeDatabase RunState

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = do
    (cleanup, clean) <- newCleanup
    return (ShakeDatabase <$> open cleanup opts (rules >> defaultRules), clean)

shakeWithDatabase :: ShakeOptions -> Rules () -> (ShakeDatabase -> IO a) -> IO a
shakeWithDatabase opts rules act = do
    (db, clean) <- shakeOpenDatabase opts rules
    (act =<< db) `finally` clean

shakeLiveFilesDatabase :: ShakeDatabase -> IO [FilePath]
shakeLiveFilesDatabase (ShakeDatabase s) = liveFilesState s

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase (ShakeDatabase s) as = do
    (refs, as) <- fmap unzip $ forM as $ \a -> do
        ref <- newIORef Nothing
        return (ref, liftIO . writeIORef ref . Just =<< a)
    after <- run s $ map void as
    results <- mapM readIORef refs
    case sequence results of
        Just result -> return (result, after)
        Nothing -> throwM $ errorInternal "Expected all results were written, but some where not"
