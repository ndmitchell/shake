{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, TupleSections #-}

module Development.Rattle.Server(
    RattleOptions(..), rattleOptions,
    Rattle, withRattle,
    cmdRattle
    ) where

import Control.Monad.Extra
import System.IO.Error
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import System.FilePath
import System.IO.Extra
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import Data.IORef
import System.Directory
import Data.List.Extra
import Data.Time
import Data.Tuple.Extra


type Args = [String]

data Cmd = Cmd
    {cmdArgs :: Args
    ,cmdRead :: [(FilePath, Maybe UTCTime)]
    ,cmdWrite :: [(FilePath, Maybe UTCTime)]
    } deriving (Show, Read)


data RattleOptions = RattleOptions
    {rattleFiles :: FilePath
    ,rattleSpeculate :: Maybe String
    ,rattleThreads :: Int
    } deriving Show

rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") 8

newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show)

data S = S
    {timestamp :: !T -- ^ The current timestamp we are on
    ,history :: [Cmd] -- ^ Commands that got run previously, in all speculation worlds
    ,speculate :: [Args] -- ^ Things that were used in the last speculation with this name
    ,running :: [(T, Args)] -- ^ Commands that are running at the moment and when they started
    ,finished :: [(T, T, Cmd)] -- ^ Commands that have finished
    ,required :: [Args] -- ^ Things what were required, not due to speculation
    }

data Rattle = Rattle
    {options :: RattleOptions
    ,state :: IORef S
    ,lock :: Lock
    }

withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = do
    history <- loadHistory options
    state <- newIORef $ S (T 0) history [] [] [] []
    lock <- newLock
    let r = Rattle{..}
    runSpeculate r
    res <- act r
    checkHazards . finished =<< readIORef state
    return res


runSpeculate :: Rattle -> IO ()
runSpeculate _ = do
    evaluate speculate -- figure out what we could do
    return ()

getTimestamp :: IORef S -> IO T
getTimestamp state =
    atomicModifyIORef' state $ \s -> (s{timestamp = succ $ timestamp s}, timestamp s)

cmdRattle :: Rattle -> Args -> IO ()
cmdRattle rattle@Rattle{..} args = do
    start <- getTimestamp state
    history <- history <$> readIORef state
    skip <- flip firstJustM history $ \cmd@Cmd{..} -> do
        let conds = (return $ args == cmdArgs) :
                    [(== time) <$> getModTime file | (file,time) <- cmdRead ++ cmdWrite]
        ifM (andM conds) (return $ Just cmd) (return Nothing)
    atomicModifyIORef' state $ \s -> (s{running = (start, args) : running s, required = args : required s}, ())
    cmd <- case skip of
        Just cmd -> return cmd
        Nothing -> do
            putStrLn $ unwords $ "#" : args
            xs :: [C.FSATrace] <- C.cmd args
            let (reads, writes) = both (nubOrd . concat) $ unzip $ map fsaRW xs
            let f xs = forM xs $ \x -> (x,) <$> getModTime x
            reads <- f reads
            writes <- f writes
            let cmd = Cmd args reads writes
            addHistory rattle cmd
            return cmd
    stop <- getTimestamp state
    atomicModifyIORef' state $ \s -> (s{finished = (start, stop, cmd) : finished s, running = filter ((/=) start . fst) $ running s}, ())
    runSpeculate rattle


getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime x = handleBool isDoesNotExistError (const $ return Nothing) (Just <$> getModificationTime x)

fsaRW :: C.FSATrace -> ([FilePath], [FilePath])
fsaRW (C.FSAWrite x) = ([], [x])
fsaRW (C.FSARead x) = ([x], [])
fsaRW (C.FSADelete x) = ([], [x])
fsaRW (C.FSAMove x y) = ([], [x,y])
fsaRW (C.FSAQuery x) = ([x], [])
fsaRW (C.FSATouch x) = ([], [x])


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


loadHistory :: RattleOptions -> IO [Cmd]
loadHistory RattleOptions{..} = do
    createDirectoryRecursive rattleFiles
    let file = rattleFiles </> "history"
    ifM (doesFileExist file) (map read . lines <$> readFile' file) (return [])

addHistory :: Rattle -> Cmd -> IO ()
addHistory Rattle{..} cmd = withLock lock $
    appendFile (rattleFiles options </> "history") $ show cmd ++ "\n"
