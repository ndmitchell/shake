{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | General rules for writing consistent rattle build systems:
--
-- * Never write to the same file twice. Never read then write.
--
-- * Don't delete files that have been produced. Each command should make
--   new files, not delete old files.
module Development.Rattle(
    rattle, Hazard,
    RattleOptions(..), rattleOptions,
    cmd,
    parallel, forP,
    liftIO
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Development.Rattle.Server
import General.Thread


newtype Run a = Run {fromRun :: ReaderT Rattle IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

parallel :: [Run a] -> Run [a]
parallel xs = do
    r <- Run ask
    liftIO $ withThreadsList $ map (flip runReaderT r . fromRun) xs

forP :: (a -> Run b) -> [a] -> Run [b]
forP f xs = parallel $ map f xs

type Args = [String]

cmd :: Args -> Run ()
cmd args = do
    r <- Run ask
    liftIO $ cmdRattle r args

-- | Given an Action to run, and a list of previous commands that got run, run it again
rattle :: RattleOptions -> Run a -> IO a
rattle opts (Run act) = withRattle opts $ \r ->
    runReaderT act r
