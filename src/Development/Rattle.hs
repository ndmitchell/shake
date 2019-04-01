{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, TupleSections #-}

module Development.Rattle(
    rattle,
    cmd,
    parallel,
    liftIO
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Extra
import System.IO.Error
import System.IO.Extra
import Control.Exception.Extra
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import Data.IORef
import System.Directory
import Data.List.Extra
import Data.Maybe
import Data.Time
import Data.Tuple.Extra


newtype Run a = Run {unRun :: ReaderT (IORef S) IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show)

data S = S
    {timestamp :: T -- the timestamp I am on
    ,running :: [Args] -- things that are running now
    ,finished :: [(T, T, Cmd)] -- people who have finished
    ,history :: [Cmd] -- what I ran last time around
    } deriving Show

getTimestamp :: Run T
getTimestamp = do
    ref <- Run ask
    liftIO $ atomicModifyIORef' ref $ \s -> (s{timestamp = succ $ timestamp s}, timestamp s)

getHistory :: Run [Cmd]
getHistory = do
    ref <- Run ask
    liftIO $ history <$> readIORef ref

addExecuted :: (T, T, Cmd) -> Run ()
addExecuted x = do
    ref <- Run ask
    liftIO $ atomicModifyIORef' ref $ \s -> (s{finished = x : finished s}, ())

parallel :: [Run a] -> Run [a]
parallel = sequence

type Args = [String]

cmd :: Args -> Run ()
cmd args = do
    start <- getTimestamp
    history <- getHistory
    skip <- liftIO $ flip firstJustM history $ \cmd@Cmd{..} -> do
        let conds = (return $ args == cmdArgs) :
                    [(== time) <$> getModTime file | (file,time) <- cmdRead ++ cmdWrite]
        ifM (andM conds) (return $ Just cmd) (return Nothing)
    cmd <- case skip of
        Just cmd -> return cmd
        Nothing -> do
            liftIO $ putStrLn $ unwords $ "#" : args
            xs :: [C.FSATrace] <- liftIO $ C.cmd args
            let (reads, writes) = both (nubOrd . concat) $ unzip $ map fsaRW xs
            let f xs = liftIO $ forM xs $ \x -> (x,) <$> getModTime x
            -- explicitly add back in the program beacuse of https://github.com/jacereda/fsatrace/issues/19
            prog <- liftIO $ case args of
                prog:_ -> findExecutable prog
                _ -> return Nothing
            reads <- f $ maybeToList prog ++ reads
            writes <- f writes
            return $ Cmd args reads writes
    stop <- getTimestamp
    addExecuted (start, stop, cmd)


getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime x = handleBool isDoesNotExistError (const $ return Nothing) (Just <$> getModificationTime x)

fsaRW :: C.FSATrace -> ([FilePath], [FilePath])
fsaRW (C.FSAWrite x) = ([], [x])
fsaRW (C.FSARead x) = ([x], [])
fsaRW (C.FSADelete x) = ([], [x])
fsaRW (C.FSAMove x y) = ([], [x,y])
fsaRW (C.FSAQuery x) = ([x], [])
fsaRW (C.FSATouch x) = ([], [x])

data Cmd = Cmd
    {cmdArgs :: Args
    ,cmdRead :: [(FilePath, Maybe UTCTime)]
    ,cmdWrite :: [(FilePath, Maybe UTCTime)]
    } deriving (Show, Read)


-- | Given an Action to run, and a list of previous commands that got run, run it again
rattle :: Run a -> IO a
rattle act = do
    history <- ifM (doesFileExist ".rattle") (map read . lines <$> readFile' ".rattle") (return [])
    ref <- newIORef $ S (T 0) [] [] history
    res <- flip runReaderT ref $ unRun act
    cmds <- finished <$> readIORef ref
    checkHazards cmds
    writeFile ".rattle" $ unlines $ reverse $ map (show . thd3) cmds
    return res

-- | You get a write/write hazard if two separate commands write to the same file.
--   You get a read/write hazard if there was a read of a file before a write.
checkHazards :: [(T, T, Cmd)] -> IO ()
checkHazards xs = do
    let writeWrite = Map.filter (\args -> length args > 1) $ Map.fromListWith (++) [(fst x, [cmdArgs]) | (_,_,Cmd{..}) <- xs, x <- cmdWrite]
    unless (Map.null writeWrite) $
        fail $ "Write/write: " ++ show writeWrite

    let lastWrite = Map.fromList [(fst x, (end, cmdArgs)) | (_,end,Cmd{..}) <- xs, x <- cmdWrite]
    let readWrite = [x | (start,_,Cmd{..}) <- xs, x <- cmdRead, Just (t, args) <- [Map.lookup (fst x) lastWrite], t >= start, args /= cmdArgs]
    unless (null readWrite) $
        fail $ "Read/write: " ++ show readWrite
