{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, TupleSections #-}

module Development.Rattle.Server(
    RattleOptions(..), rattleOptions,
    Rattle, withRattle,
    cmdRattle
    ) where

import Control.Monad.IO.Class
import Control.Monad.Extra
import System.IO.Error
import Control.Exception.Extra
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

data Rattle = Rattle RattleOptions (IORef S)

withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle opts@RattleOptions{..} act = do
    ref <- newIORef $ S (T 0) [] [] [] [] []
    let r = Rattle opts ref
    runSpeculate r
    res <- act r
    checkHazards . finished =<< readIORef ref
    return res


runSpeculate :: Rattle -> IO ()
runSpeculate _ = do
    evaluate speculate -- figure out what we could do
    return ()

getTimestamp :: IORef S -> IO T
getTimestamp ref = do
    atomicModifyIORef' ref $ \s -> (s{timestamp = succ $ timestamp s}, timestamp s)

cmdRattle :: Rattle -> Args -> IO ()
cmdRattle (Rattle opts ref) args = do
    start <- getTimestamp ref
    history <- history <$> readIORef ref
    skip <- liftIO $ flip firstJustM history $ \cmd@Cmd{..} -> do
        let conds = (return $ args == cmdArgs) :
                    [(== time) <$> getModTime file | (file,time) <- cmdRead ++ cmdWrite]
        ifM (andM conds) (return $ Just cmd) (return Nothing)
    atomicModifyIORef' ref $ \s -> (s{running = (start, args) : running s, required = args : required s}, ())
    cmd <- case skip of
        Just cmd -> return cmd
        Nothing -> do
            liftIO $ putStrLn $ unwords $ "#" : args
            xs :: [C.FSATrace] <- liftIO $ C.cmd args
            let (reads, writes) = both (nubOrd . concat) $ unzip $ map fsaRW xs
            let f xs = liftIO $ forM xs $ \x -> (x,) <$> getModTime x
            -- explicitly add back in the program beacuse of https://github.com/jacereda/fsatrace/issues/19
            reads <- f reads
            writes <- f writes
            return $ Cmd args reads writes
    stop <- getTimestamp ref
    atomicModifyIORef' ref $ \s -> (s{finished = (start, stop, cmd) : finished s, running = filter ((/=) start . fst) $ running s}, ())

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
