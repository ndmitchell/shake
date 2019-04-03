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
import Data.Char
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import System.Directory
import Data.Hashable
import Numeric
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

data Reason = Speculative | Required

data S = S
    {timestamp :: !T -- ^ The current timestamp we are on
    ,history :: [Cmd] -- ^ Commands that got run previously, in all speculation worlds
    ,speculate :: [Args] -- ^ Things that were used in the last speculation with this name
    ,running :: [(Reason, T, Args)] -- ^ Commands that are running at the moment and when they started
    ,finished :: [(Reason, T, T, Cmd)] -- ^ Commands that have finished
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
    speculate <- loadSpeculate options
    state <- newIORef $ S (T 0) history speculate [] [] []
    lock <- newLock
    let r = Rattle{..}
    runSpeculate r
    res <- act r
    checkHazards . finished =<< readIORef state
    saveSpeculate r
    return res


runSpeculate :: Rattle -> IO ()
runSpeculate Rattle{..} =
    -- speculate on a process iff it is the first process in speculate that:
    -- 1) we have some parallelism free
    -- 2) it is the first eligible in the list
    -- 3) not running or finished
    -- 4) no write conflicts with anything currently running
    -- 5) no read conflicts with anything running or any earlier speculation
    join $ atomicModifyIORef' state $ \s -> case nextSpeculate s of
        Just x | length (running s) < rattleThreads options ->
            (s, return ())
        _ -> (s, return ())


nextSpeculate :: S -> Maybe Args
nextSpeculate S{..} = do w <- runningWrites; f w speculate
    where
        getReads x  = if null prev then Nothing else Just $ map fst $ concatMap cmdRead prev
            where prev = filter ((==) x . cmdArgs) history
        getWrites x  = if null prev then Nothing else Just $ map fst $ concatMap cmdWrite prev
            where prev = filter ((==) x . cmdArgs) history

        runningOrFinished = Set.fromList $ [x | (_,_,x) <- running] ++ [cmdArgs x | (_,_,_,x) <- finished]
        runningWrites = Set.fromList <$> concatMapM (getWrites . thd3) running

        f possWrite [] = Nothing
        f possWrite (x:xs)
            | not $ x `Set.member` runningOrFinished
            , Just wRun <- runningWrites, Just wMe <- getWrites x, not $ any (`Set.member` wRun) wMe
            , Just rMe <- getReads x, not $ any (`Set.member` possWrite) rMe
                = Just x
            | Just wMe <- getWrites x
                = f (Set.fromList wMe `Set.union` possWrite) xs
            | otherwise = Nothing



getTimestamp :: IORef S -> IO T
getTimestamp state =
    atomicModifyIORef' state $ \s -> (s{timestamp = succ $ timestamp s}, timestamp s)

cmdRattle :: Rattle -> Args -> IO ()
cmdRattle rattle@Rattle{..} args = do
    start <- getTimestamp state
    run <- atomicModifyIORef' state $ \s ->
        if args `elem` ([x | (_,_,x) <- running s] ++ [cmdArgs x | (_,_,_,x) <- finished s])
            then (s{required = args : required s}, False)
            else (s{running = (Required, start, args) : running s, required = args : required s}, True)
    when run $ do
        history <- history <$> readIORef state
        skip <- flip firstJustM history $ \cmd@Cmd{..} -> do
            let conds = (return $ args == cmdArgs) :
                        [(== time) <$> getModTime file | (file,time) <- cmdRead ++ cmdWrite]
            ifM (andM conds) (return $ Just cmd) (return Nothing)
        cmd <- case skip of
            Just cmd ->
                -- if we are hitting the cache and its the same then:
                -- 1) don't store it to the history (it's already in there)
                -- 2) we aren't really writing to it, we're using the value of the writes, so don't conflict
                return cmd{cmdWrite=[], cmdRead=cmdWrite cmd ++ cmdRead cmd}
            Nothing -> do
                runSpeculate rattle
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
        atomicModifyIORef' state $ \s -> (s{finished = (Required, start, stop, cmd) : finished s, running = filter ((/=) start . snd3) $ running s}, ())
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
checkHazards :: [(Reason, T, T, Cmd)] -> IO ()
checkHazards xs = do
    let writeWrite = Map.filter (\args -> length args > 1) $ Map.fromListWith (++) [(fst x, [cmdArgs]) | (_,_,_,Cmd{..}) <- xs, x <- cmdWrite]
    unless (Map.null writeWrite) $
        fail $ "Write/write: " ++ show writeWrite

    let lastWrite = Map.fromList [(fst x, (end, cmdArgs)) | (_,_,end,Cmd{..}) <- xs, x <- cmdWrite]
    let readWrite = [x | (_,start,_,Cmd{..}) <- xs, x <- cmdRead, Just (t, args) <- [Map.lookup (fst x) lastWrite], t >= start, args /= cmdArgs]
    unless (null readWrite) $
        fail $ "Read/write: " ++ show readWrite


loadFile :: RattleOptions -> String -> IO String
loadFile RattleOptions{..} name = do
    createDirectoryRecursive rattleFiles
    let file = rattleFiles </> name
    ifM (doesFileExist file) (readFile' file) (return "")

loadHistory :: RattleOptions -> IO [Cmd]
loadHistory options = map read . lines <$> loadFile options "history"

addHistory :: Rattle -> Cmd -> IO ()
addHistory Rattle{..} cmd = withLock lock $
    appendFile (rattleFiles options </> "history") $ show cmd ++ "\n"


speculateName :: RattleOptions -> Maybe String
speculateName RattleOptions{..} = f <$> rattleSpeculate
    where f x = "speculate-" ++ take 25 (filter isAlphaNum x) ++ "-" ++ showHex (hash x) ""

loadSpeculate :: RattleOptions -> IO [Args]
loadSpeculate options
    | Just name <- speculateName options = map read . lines <$> loadFile options name
    | otherwise = return []

saveSpeculate :: Rattle -> IO ()
saveSpeculate Rattle{..} = whenJust (speculateName options) $ \name -> do
    ref <- readIORef state
    writeFile (rattleFiles options </> name) $ unlines $ map show $ nubOrd $ reverse $ required ref
