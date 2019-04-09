{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Rattle.Server(
    RattleOptions(..), rattleOptions,
    Rattle, withRattle,
    Hazard,
    cmdRattle
    ) where

import Control.Monad.Extra
import Development.Rattle.Limit
import Development.Rattle.Types
import Development.Rattle.Shared
import Development.Rattle.Hash
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.Hashable
import Data.List.Extra
import Data.Tuple.Extra


data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    } deriving Show

rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") True 8

data ReadOrWrite = Read | Write deriving (Show,Eq)

data S = S
    {timestamp :: !T
        -- ^ The current timestamp we are on
    ,started :: Map.HashMap Cmd (NoShow (IO ()))
        -- ^ Things that have got to running - if you find a duplicate just run the IO
        --   to wait for it.
    ,running :: [(T, Cmd, [Trace ()])]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: Map.HashMap FilePath (ReadOrWrite, T, Cmd)
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(T, Cmd, Trace Hash)]
        -- ^ Things that have completed, and would like to get recorded, but have to wait
        --   to confirm they didn't cause hazards
    ,required :: [Cmd]
        -- ^ Things what were required by the user calling cmdRattle, not added due to speculation.
        --   Will be the 'speculate' list next time around.
    } deriving Show


data Problem
    = Finished
    | Hazard Hazard

throwProblem :: Problem -> IO a
throwProblem Finished = fail "Finished, but still trying to do stuff"
throwProblem (Hazard h) = throwIO h

data Hazard
    = ReadWriteHazard FilePath Cmd Cmd
    | WriteWriteHazard FilePath Cmd Cmd
      deriving Show
instance Exception Hazard


data Rattle = Rattle
    {options :: RattleOptions
    ,speculate :: [(Cmd, [Trace Hash])] -- ^ Things that were used in the last speculation with this name
    ,state :: Var (Either Problem S)
    ,speculated :: IORef Bool
    ,limit :: Limit
    ,shared :: Shared
    }


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withShared rattleFiles $ \shared -> do
    speculate <- maybe (return []) (getSpeculate shared) rattleSpeculate
    speculate <- fmap (takeWhile (not . null . snd)) $ forM speculate $ \x -> (x,) <$> unsafeInterleaveIO (getCmdTraces shared x)
    speculated <- newIORef False
    let s0 = Right $ S t0 Map.empty [] Map.empty [] []
    state <- newVar s0
    limit <- newLimit rattleProcesses
    let r = Rattle{..}
    runSpeculate r

    let saveSpeculate state =
            whenJust rattleSpeculate $ \name ->
                whenRightM (readVar state) $ \v ->
                    setSpeculate shared name $ reverse $ required v

    -- first try and run it
    ((act r <* saveSpeculate state) `finally` writeVar state (Left Finished)) `catch`
        \(h :: Hazard) -> do
            b <- readIORef speculated
            if not b then throwIO h else do
                -- if we speculated, and we failed with a hazard, try again
                putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
                print h
                state <- newVar s0
                limit <- newLimit rattleProcesses
                let r = Rattle{speculate=[], ..}
                (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)


runSpeculate :: Rattle -> IO ()
runSpeculate rattle@Rattle{..} = void $ withLimitMaybe limit $ forkIO $
    -- speculate on a process iff it is the first process in speculate that:
    -- 1) we have some parallelism free
    -- 2) it is the first eligible in the list
    -- 3) not already been started
    -- 4) no read/write conflicts with anything completed
    -- 5) no read conflicts with anything running or any earlier speculation
    join $ modifyVar state $ \s -> case s of
        Right s | Just cmd <- nextSpeculate rattle s -> do
            writeIORef speculated True
            cmdRattleStarted rattle cmd s ["speculating"]
        _ -> return (s,  return ())


nextSpeculate :: Rattle -> S -> Maybe Cmd
nextSpeculate Rattle{..} S{..}
    | any (null . thd3) running = Nothing
    | otherwise = step (addTrace (Set.empty, Set.empty) $ mconcat $ concatMap thd3 running) speculate
    where
        addTrace (r,w) Trace{..} = (f r tRead, f w tWrite)
            where f set xs = Set.union set $ Set.fromList $ map fst xs

        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw@(r, w) ((x, mconcat -> t@Trace{..}):xs)
            | not $ any (\v -> v `Set.member` r || v `Set.member` w || v `Map.member` hazard) $ map fst tWrite
                -- if anyone I write has ever been read or written, or might be by an ongoing thing, that would be bad
            , not $ any (`Set.member` w) $ map fst tRead
                -- if anyone I read might be being written right now, that would be bad
                = Just x
            | otherwise
                = step (addTrace rw t) xs


cmdRattle :: Rattle -> [String] -> IO ()
cmdRattle rattle args = cmdRattleRequired rattle $ Cmd args

cmdRattleRequired :: Rattle -> Cmd -> IO ()
cmdRattleRequired rattle@Rattle{..} cmd = withLimit limit $ do
    modifyVar_ state $ return . fmap (\s -> s{required = cmd : required s})
    cmdRattleStart rattle cmd

cmdRattleStart :: Rattle -> Cmd -> IO ()
cmdRattleStart rattle@Rattle{..} cmd = join $ modifyVar state $ \s -> case s of
    Left e -> throwProblem e
    Right s -> cmdRattleStarted rattle cmd s []

cmdRattleStarted :: Rattle -> Cmd -> S -> [String] -> IO (Either Problem S, IO ())
cmdRattleStarted rattle@Rattle{..} cmd s msgs = do
    let start = timestamp s
    s <- return s{timestamp = succ $ timestamp s}
    case Map.lookup cmd (started s) of
        Just (NoShow wait) -> return (Right s, wait)
        Nothing -> do
            hist <- unsafeInterleaveIO $ getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs
            s <- return s{running = (start, cmd, map void hist) : running s}
            s <- return s{started = Map.insert cmd (NoShow go) $ started s}
            return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)


-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> T -> [Trace Hash] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd args) start hist msgs = do
    hasher <- memoIO hashFile
    let match (fp, h) = (== Just h) <$> hasher fp
    histRead <- filterM (allM match . tRead) hist
    histBoth <- filterM (allM match . tWrite) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle start cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, h) = do v <- fetcher h; case v of Nothing -> return Nothing; Just op -> return $ Just $ op fp
            download <- if not (rattleShare options)
                then return Nothing
                else firstJustM (\t -> fmap (t,) <$> allMaybeM fetch (tWrite t)) histRead
            case download of
                Just (t, download) -> do
                    display ["copying"]
                    sequence_ download
                    cmdRattleFinished rattle start cmd t False
                Nothing -> do
                    display []
                    t <- fsaTrace <$> C.cmd args
                    let skip x = "/dev/" `isPrefixOf` x || hasTrailingPathSeparator x
                    let f xs = mapMaybeM (\x -> fmap (x,) <$> hashFile x) $ filter (not . skip) $ map fst xs
                    t <- Trace <$> f (tRead t) <*> f (tWrite t)
                    when (rattleShare options) $
                        forM_ (tWrite t) $ \(fp, h) ->
                            setFile shared fp h ((== Just h) <$> hashFile fp)
                    cmdRattleFinished rattle start cmd t True
    where
        display msgs2 = BS.putStrLn $ BS.pack $ unwords $ "#" : args ++ ["(" ++ unwords (msgs ++ msgs2) ++ ")" | not $ null $ msgs ++ msgs2]

-- | I finished running a command
cmdRattleFinished :: Rattle -> T -> Cmd -> Trace Hash -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyVar state $ \s -> case s of
    Left e -> throwProblem e
    Right s -> do
        -- update all the invariants
        let stop = timestamp s
        s <- return s{timestamp = succ $ timestamp s}
        s <- return s{running = filter ((/= start) . fst3) $ running s}
        s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s}

        -- look for hazards
        let newHazards = Map.fromList $ map ((,(Write,start,cmd)) . fst) tWrite ++
                                        map ((,(Read ,stop ,cmd)) . fst) tRead
        case unionWithKeyEithers mergeFileOps (hazard s) newHazards of
            (ps@(p:_), _) -> return (Left $ Hazard p, print ps >> throwIO p)
            ([], hazard2) -> do
                s <- return s{hazard = hazard2}

                -- move people out of pending if they have survived long enough
                let earliest = minimum $ succ stop : map fst3 (running s)
                (safe, pending) <- return $ partition (\x -> fst3 x < earliest) $ pending s
                s <- return s{pending = pending}
                return (Right s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c t)


mergeFileOps :: FilePath -> (ReadOrWrite, T, Cmd) -> (ReadOrWrite, T, Cmd) -> Either Hazard (ReadOrWrite, T, Cmd)
mergeFileOps x (Read, t1, cmd1) (Read, t2, cmd2) = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps x (Write, t1, cmd1) (Write, t2, cmd2) = Left $ WriteWriteHazard x cmd1 cmd2
mergeFileOps x (Read, t1, cmd1) (Write, t2, cmd2)
    | t1 <= t2 = Left $ ReadWriteHazard x cmd2 cmd1
    | otherwise = Right (Write, t2, cmd2)
mergeFileOps x v1 v2 = mergeFileOps x v2 v1 -- must be Write/Read, so match the other way around


allMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allMaybeM f [] = return $ Just []
allMaybeM f (x:xs) = do
    y <- f x
    case y of
        Nothing -> return Nothing
        Just y -> fmap (y:) <$> allMaybeM f xs


unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e v) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp)
            Just v1 -> case op k v1 v2 of
                Left e -> (e:es, mp)
                Right v -> (es, Map.insert k v mp)
