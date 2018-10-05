
-- | This module reexports the six necessary type classes that every 'Rule' type must support.
--   You can use this module to define new rules without depending on the @binary@, @deepseq@ and @hashable@ packages.
module Development.Shake.Database(
    ShakeDatabase,
    shakeOpenDatabase,
    shakeWithDatabase,
    shakeRunDatabase,
    shakeLiveFilesDatabase,
    shakeRunAfters
    ) where

import Control.Exception
import Data.Functor
import General.Cleanup
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Rules.Default


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
    after <- run s $ map void as
    return ([], after)
