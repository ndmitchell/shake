{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, TupleSections #-}

module Development.Rattle(
    rattle,
    RattleOptions(..), rattleOptions,
    cmd,
    parallel,
    liftIO
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Development.Rattle.Server


newtype Run a = Run (ReaderT Rattle IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

parallel :: [Run a] -> Run [a]
parallel = sequence

type Args = [String]

cmd :: Args -> Run ()
cmd args = do
    r <- Run ask
    liftIO $ cmdRattle r args


-- | Given an Action to run, and a list of previous commands that got run, run it again
rattle :: RattleOptions -> Run a -> IO a
rattle opts (Run act) = withRattle opts $ \r ->
    runReaderT act r
