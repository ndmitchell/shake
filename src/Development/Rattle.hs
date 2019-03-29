{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}

module Development.Rattle(
    rattle,
    cmd,
    parallel,
    liftIO
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.List.Extra
import Data.Tuple.Extra


newtype Run a = Run {unRun :: ReaderT (IORef S) IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show)

data S = S
    {timestamp :: T -- the timestamp I am on
    ,executed :: [(T, T, Cmd)]
    } deriving Show

getTimestamp :: Run T
getTimestamp = do
    ref <- Run ask
    liftIO $ atomicModifyIORef' ref $ \s -> (s{timestamp = succ $ timestamp s}, timestamp s)

addExecuted :: (T, T, Cmd) -> Run ()
addExecuted x = do
    ref <- Run ask
    liftIO $ atomicModifyIORef' ref $ \s -> (s{executed = x : executed s}, ())

parallel :: [Run a] -> Run [a]
parallel = sequence

type Args = [String]

cmd :: Args -> Run ()
cmd args = do
    start <- getTimestamp
    xs :: [C.FSATrace] <- liftIO $ C.cmd args
    stop <- getTimestamp
    let (reads, writes) = both (nubOrd . concat) $ unzip $ map fsaRW xs
    addExecuted (start, stop, Cmd args reads writes)

fsaRW :: C.FSATrace -> ([FilePath], [FilePath])
fsaRW (C.FSAWrite x) = ([], [x])
fsaRW (C.FSARead x) = ([x], [])
fsaRW (C.FSADelete x) = ([], [x])
fsaRW (C.FSAMove x y) = ([], [x,y])
fsaRW (C.FSAQuery x) = ([x], [])
fsaRW (C.FSATouch x) = ([], [x])


data Cmd = Cmd
    {cmdArgs :: Args
    ,cmdRead :: [FilePath]
    ,cmdWrite :: [FilePath]
    } deriving Show

-- | Given an Action to run, and a list of previous commands that got run, run it again
rattle :: Run a -> IO a
rattle act = do
    ref <- newIORef $ S (T 0) []
    res <- flip runReaderT ref $ unRun act
    cmds <- executed <$> readIORef ref
    checkHazards cmds
    return res

-- | You get a write/write hazard if two separate commands write to the same file.
--   You get a read/write hazard if there was a read of a file before a write.
checkHazards :: [(T, T, Cmd)] -> IO ()
checkHazards xs = do
    let writeWrite = Map.filter (\args -> length args > 1) $ Map.fromListWith (++) [(x, [cmdArgs]) | (_,_,Cmd{..}) <- xs, x <- cmdWrite]
    unless (Map.null writeWrite) $
        fail $ "Write/write: " ++ show writeWrite

    let lastWrite = Map.fromList [(x, (end, cmdArgs)) | (_,end,Cmd{..}) <- xs, x <- cmdWrite]
    let readWrite = [x | (start,_,Cmd{..}) <- xs, x <- cmdRead, Just (t, args) <- [Map.lookup x lastWrite], t >= start, args /= cmdArgs]
    unless (null readWrite) $
        fail $ "Read/write: " ++ show readWrite
