{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-
Files stores the meta-data so its very important its always accurate
We can't rely on getting a Ctrl+C at the end, so we'd better write out a journal
But if we do happen to get a Ctrl+C then that is very handy
The journal is idempotent, i.e. if we replay the journal twice all is good
-}

module Development.Shake.Database(
    Time, startTime, Duration, Trace,
    Database, withDatabase,
    Ops(..), eval,
    allEntries, showJSON,
    ) where

import Development.Shake.Binary
import Development.Shake.Pool
import Development.Shake.Value
import Development.Shake.Locks

import Prelude hiding (catch)
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe
import Data.List
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO

-- we want readFile/writeFile to be byte orientated, not do windows line end conversion
import qualified Data.ByteString.Lazy as LBS (readFile,writeFile)
import qualified Data.ByteString.Lazy.Char8 as LBS hiding (readFile,writeFile)


-- Increment every time the on-disk format/semantics change,
-- @i@ is for the users version number
databaseVersion i = "SHAKE-DATABASE-2-" ++ show (i :: Int) ++ "\r\n"
journalVersion i = "SHAKE-JOURNAL-2-" ++ show (i :: Int) ++ "\r\n"


---------------------------------------------------------------------
-- UTILITY TYPES AND FUNCTIONS

removeFile_ :: FilePath -> IO ()
removeFile_ x = catch (removeFile x) (\(e :: SomeException) -> return ())


type Map = Map.HashMap


newtype Step = Step Int deriving (Eq,Ord,Show)

incStep (Step i) = Step $ i + 1


type Duration = Double -- duration in seconds

duration :: IO a -> IO (Duration, a)
duration act = do
    start <- getCurrentTime
    res <- act
    end <- getCurrentTime
    return (fromRational $ toRational $ end `diffUTCTime` start, res)


type Time = Double -- how far you are through this run, in seconds

-- | Call once at the start, then call repeatedly to get Time values out
startTime :: IO (IO Time)
startTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ fromRational $ toRational $ end `diffUTCTime` start


---------------------------------------------------------------------
-- CENTRAL TYPES

type Trace = (String, Time, Time)


-- | Invariant: The database does not have any cycles when a Key depends on itself
data Database = Database
    {lock :: Lock
    ,status :: IORef (Map Key Status)
    ,step :: Step
    ,journal :: Journal
    ,filename :: FilePath
    ,version :: Int -- user supplied version
    ,logger :: String -> IO () -- logging function
    }

data Status
    = Ready Result -- I have a value
    | Error SomeException -- I have been run and raised an error
    | Loaded Result -- Loaded from the database
    | Dirty (Maybe Result) -- One of my dependents is in Error or Dirty
    | Checking Pending Result -- Currently checking if I am valid
    | Building Pending (Maybe Result) -- Currently building
      deriving Show

data Result = Result
    {value :: Value -- the value associated with the Key
    ,built :: Step -- when it was actually run
    ,changed :: Step -- the step for deciding if it's valid
    ,depends :: [[Key]] -- dependencies
    ,execution :: Duration -- how long it took when it was last run (seconds)
    ,traces :: [Trace] -- a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving Show

newtype Pending = Pending (IORef (IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error, Ready or Dirty (checking only)

addPending :: Pending -> IO () -> IO ()
addPending (Pending p) act = modifyIORef p (>> act)

instance Show Pending where show _ = "Pending"


getResult :: Status -> Maybe Result
getResult (Ready r) = Just r
getResult (Loaded r) = Just r
getResult (Checking _ r) = Just r
getResult (Dirty r) = r
getResult (Building _ r) = r
getResult _ = Nothing

getPending :: Status -> Maybe Pending
getPending (Checking p _) = Just p
getPending (Building p _) = Just p
getPending _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

data Ops = Ops
    {valid :: Key -> Value -> IO Bool
        -- ^ Given a Key and a Value from the database, check it still matches the value stored on disk
    ,exec :: [Key] -> Key -> IO (Either SomeException (Value, [[Key]], Duration, [Trace]))
        -- ^ Given a chunk of stack (bottom element first), and a key, either raise an exception or successfully build it
    ,diagnostics :: String -> IO ()
        -- ^ Print out a diagnostics message (usually just ignores the message)
    }


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
eval :: Pool -> Database -> Ops -> [Key] -> IO (Either SomeException (Duration,[Value]))
eval pool Database{..} Ops{..} ks =
    join $ withLock lock $ do
        vs <- mapM (evalRECB []) ks
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ return $ Right (0, [value r | Ready r <- vs])
         else if not $ null errs then
            return $ return $ Left $ head errs
         else do
            let cb = filter (isCheckingBuilding . snd) $ zip ks vs
            wait <- newBarrier
            todo <- newIORef $ Just $ length cb
            forM_ cb $ \(k,v) -> do
                let act = do
                        t <- readIORef todo
                        when (isJust t) $ do
                            s <- readIORef status
                            case Map.lookup k s of
                                Just (Error e) -> do
                                    writeIORef todo Nothing
                                    signalBarrier wait $ Left e
                                Just (Dirty r) -> do
                                    Building p _ <- evalB [] k r
                                    addPending p act -- try again
                                Just (Building p _) -> do
                                    -- can only happen if two people are waiting on the same Dirty
                                    -- the first gets kicked, and sets it to Building, meaning the second sees Building
                                    -- very subtle!
                                    addPending p act
                                Just Ready{} | fromJust t == 1 -> do
                                    writeIORef todo Nothing
                                    signalBarrier wait $ Right [value r | k <- ks, let Ready r = fromJust $ Map.lookup k s]
                                Just Ready{} ->
                                    writeIORef todo $ fmap (subtract 1) t
                addPending (fromJust $ getPending v) act
            return $ do
                (dur,res) <- duration $ blockPool pool $ waitBarrier wait
                return $ case res of
                    Left e -> Left e
                    Right v -> Right (dur,v)
    where
        k #= v = do
            s <- readIORef status
            writeIORef status $ Map.insert k v s
            let shw = head . words . show
            diagnostics $ maybe "Missing" shw (Map.lookup k s) ++ " -> " ++ shw v ++ ", " ++ show k
            return v

        isErrorDirty Error{} = True
        isErrorDirty Dirty{} = True
        isErrorDirty _ = False

        isCheckingBuilding Checking{} = True
        isCheckingBuilding Building{} = True
        isCheckingBuilding _ = False

        isReady Ready{} = True
        isReady _ = False

        -- Rules for each eval* function
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must return only one of the items in its suffix


        evalRECB :: [Key] -> Key -> IO Status
        evalRECB stack k = do
            res <- evalREDCB stack k
            case res of
                Dirty r -> evalB stack k r
                res -> return res


        evalB :: [Key] -> Key -> Maybe Result -> IO Status
        evalB stack k r = do
            pend <- newIORef (return ())
            addPool pool $ do
                res <- exec stack k
                ans <- withLock lock $ do
                    ans <- k #= case res of
                        Left err -> Error err
                        Right (v,depends,execution,traces) ->
                            let c | Just r <- r, value r == v = changed r
                                  | otherwise = step
                            in Ready $ Result{value=v,changed=c,built=step,..}
                    join $ readIORef pend
                    return ans
                case ans of
                    Ready r -> appendJournal journal k r -- leave the DB lock before appending
                    _ -> return ()
            k #= Building (Pending pend) r


        evalREDCB :: [Key] -> Key -> IO Status
        evalREDCB stack k = do
            s <- readIORef status
            case Map.lookup k s of
                Nothing -> evalB stack k Nothing
                Just (Loaded r) -> do
                    b <- valid k (value r)
                    if not b then evalB stack k $ Just r else checkREDCB stack k r (depends r)
                Just res -> return res


        checkREDCB :: [Key] -> Key -> Result -> [[Key]] -> IO Status
        checkREDCB stack k r [] = do
            k #= Ready r
        checkREDCB stack k r (ds:rest) = do
            vs <- mapM (evalREDCB (k:stack)) ds
            let cb = filter (isCheckingBuilding . snd) $ zip ds vs
            if any isErrorDirty vs then
                k #= Dirty (Just r)
             else if any (> built r) [changed | Ready Result{..} <- vs] then
                evalB stack k $ Just r
             else if null cb then
                checkREDCB stack k r rest
             else do
                todo <- newIORef $ Just $ length cb -- how many to do
                pend <- newIORef $ return ()
                forM_ cb $ \(d,i) ->
                    addPending (fromJust $ getPending i) $ do
                        t <- readIORef todo
                        when (isJust t) $ do
                            s <- readIORef status
                            case Map.lookup d s of
                                Just v | isErrorDirty v -> do
                                    writeIORef todo Nothing
                                    k #= Dirty (Just r)
                                    join $ readIORef pend
                                Just (Ready r2)
                                    | changed r2 > built r -> do
                                        writeIORef todo Nothing
                                        Building p _ <- evalB stack k $ Just r
                                        addPending p $ join $ readIORef pend
                                    | fromJust t == 1 -> do
                                        writeIORef todo Nothing
                                        res <- checkREDCB stack k r rest
                                        case getPending res of
                                            Nothing -> join $ readIORef pend
                                            Just p -> addPending p $ join $ readIORef pend
                                    | otherwise ->
                                        writeIORef todo $ fmap (subtract 1) t
                                Just (Building p _) -> do
                                    writeIORef todo Nothing
                                    addPending p $ join $ readIORef pend
                k #= Checking (Pending pend) r


---------------------------------------------------------------------
-- QUERY DATABASE

-- | Return a list of keys in an order which would build them bottom up. Relies on the invariant
--   that the database is not cyclic.
allEntries :: Database -> IO [(Key,Value)]
allEntries Database{..} = do
    status <- readIORef status
    return $ ordering [((k, value i), concat $ depends i) | (k,v) <- Map.toList status, Just i <- [getResult v]]
    where
        ordering :: Eq a => [((a,b), [a])] -> [(a,b)]
        ordering xs = f [(a, nub b `intersect` as) | let as = map (fst . fst) xs, (a,b) <- xs]
            where
                f xs | null xs = []
                     | null now = error "Internal invariant broken, database seems to be cyclic (probably during lint)"
                     | otherwise = let ns = map fst now in ns ++ f [(a,b \\ map fst ns) | (a,b) <- later]
                    where (now,later) = partition (null . snd) xs


showJSON :: Database -> IO String
showJSON Database{..} = do
    status <- readIORef status
    let ids = Map.fromList $ zip (Map.keys status) [0..]
        f (k, v) | Just Result{..} <- getResult v =
            let xs = ["name:" ++ show (show k)
                     ,"built:" ++ showStep built
                     ,"changed:" ++ showStep changed
                     ,"depends:" ++ show (mapMaybe (`Map.lookup` ids) (concat depends))
                     ,"execution:" ++ show execution] ++
                     ["traces:[" ++ intercalate "," (map showTrace traces) ++ "]" | traces /= []]
                showStep (Step i) = show i
                showTrace (a,b,c) = "{start:" ++ show b ++ ",stop:" ++ show c ++ ",command:" ++ show a ++ "}"
            in  ["{" ++ intercalate ", " xs ++ "}"]
        f _ = []
    return $ "[" ++ intercalate "\n," (concatMap f $ Map.toList status) ++ "\n]"


---------------------------------------------------------------------
-- DATABASE

withDatabase :: (String -> IO ()) -> FilePath -> Int -> (Database -> IO a) -> IO a
withDatabase logger filename version = bracket (openDatabase logger filename version) closeDatabase


-- Files are named based on the FilePath, but with different extensions,
-- such as .database, .journal, .trace
openDatabase :: (String -> IO ()) -> FilePath -> Int -> IO Database
openDatabase logger filename version = do
    let dbfile = filename <.> "database"
        jfile = filename <.> "journal"

    lock <- newLock
    (step, status) <- readDatabase dbfile version
    step <- return $ incStep step
    
    b <- doesFileExist jfile
    (status,step) <- if not b then return (status,step) else do
        status <- replayJournal jfile version status
        removeFile_ jfile
        -- the journal potentially things at the current step, so increment my step
        writeDatabase dbfile version step status
        return (status, incStep step)

    status <- newIORef status
    journal <- openJournal jfile version
    return Database{..}


closeDatabase :: Database -> IO ()
closeDatabase Database{..} = do
    status <- readIORef status
    writeDatabase (filename <.> "database") version step status
    closeJournal journal


writeDatabase :: FilePath -> Int -> Step -> Map Key Status -> IO ()
writeDatabase file version step status = do
    ws <- currentWitness
    LBS.writeFile file $
        (LBS.pack $ databaseVersion version) `LBS.append`
        encode (step, Witnessed ws $ Statuses status)


readDatabase :: FilePath -> Int -> IO (Step, Map Key Status)
readDatabase file version = do
    let zero = (Step 1, Map.fromList [])
    b <- doesFileExist file
    if not b
        then return zero
        else catch (do
            src <- readFileVer file $ databaseVersion version
            let (a,b) = decode src
                c = fromStatuses $ fromWitnessed b
            -- FIXME: The LBS.length shouldn't be necessary, but it is
            a `seq` c `seq` LBS.length src `seq` return (a,c)) $
            \(err :: SomeException) -> do
                putStrLn $ unlines $
                    ("Error when reading Shake database " ++ file) :
                    map ("  "++) (lines $ show err) ++
                    ["All files will be rebuilt"]
                removeFile_ file -- so it doesn't error next time
                return zero


---------------------------------------------------------------------
-- JOURNAL

data Journal = Journal
    {handle :: Var (Maybe Handle)
    ,journalFile :: FilePath
    ,witness :: Witness
    }

openJournal :: FilePath -> Int -> IO Journal
openJournal journalFile ver = do
    h <- openBinaryFile journalFile WriteMode
    hSetFileSize h 0
    LBS.hPut h $ LBS.pack $ journalVersion ver
    witness <- currentWitness
    writeChunk h $ encode witness
    hFlush h
    handle <- newVar $ Just h
    return Journal{..}


replayJournal :: FilePath -> Int -> Map Key Status -> IO (Map Key Status)
replayJournal file ver mp = catch (do
    src <- readFileVer file $ journalVersion ver
    let ws:rest = readChunks src
    ws :: Witness <- return $ decode ws
    rest <- return $ map (runGet (getWith ws)) rest
    return $ foldl' (\mp (k,v) -> Map.insert k (Loaded v) mp) mp rest)
    $ \(err :: SomeException) -> do
        putStrLn $ unlines $
            ("Error when reading Shake journal " ++ file) :
            map ("  "++) (lines $ show err) ++
            ["All files built in the last exceution will be rebuilt"]
        return mp


appendJournal :: Journal -> Key -> Result -> IO ()
appendJournal Journal{..} k i = modifyVar_ handle $ \v -> case v of
    Nothing -> return Nothing
    Just h -> do
        writeChunk h $ runPut $ putWith witness (k,i)
        return $ Just h


closeJournal :: Journal -> IO ()
closeJournal Journal{..} =
    modifyVar_ handle $ \v -> case v of
        Nothing -> return Nothing
        Just h -> do
            hClose h
            removeFile_ journalFile
            return Nothing


---------------------------------------------------------------------
-- SERIALISATION

instance Binary Step where
    put (Step i) = put i
    get = fmap Step get


data Witnessed a = Witnessed Witness a
fromWitnessed (Witnessed _ x) = x

instance BinaryWith Witness a => Binary (Witnessed a) where
    put (Witnessed ws x) = put ws >> putWith ws x
    get = do ws <- get; x <- getWith ws; return $ Witnessed ws x

-- Only for serialisation
newtype Statuses = Statuses {fromStatuses :: Map Key Status}

instance BinaryWith Witness Statuses where
    putWith ws (Statuses x) = putWith ws [(k,i) | (k,v) <- Map.toList x, Just i <- [getResult v]]
    getWith ws = do
        x <- getWith ws
        return $ Statuses $ Map.fromList $ map (second Loaded) x

instance BinaryWith Witness Result where
    putWith ws (Result x1 x2 x3 x4 x5 x6) = putWith ws x1 >> put x2 >> put x3 >> putWith ws x4 >> put x5 >> put x6
    getWith ws = do x1 <- getWith ws; x2 <- get; x3 <- get; x4 <- getWith ws; x5 <- get; x6 <- get; return $ Result x1 x2 x3 x4 x5 x6


readFileVer :: FilePath -> String -> IO LBS.ByteString
readFileVer file ver = do
    let ver2 = LBS.pack ver
    src <- LBS.readFile file
    unless (ver2 `LBS.isPrefixOf` src) $ do
        let bad = LBS.takeWhile (\x -> isAlphaNum x || x `elem` "-_ ") $ LBS.take 50 src
        error $ "Invalid version stamp\n" ++
                "Expected: " ++ ver ++ "\n" ++
                "Got     : " ++ LBS.unpack bad
    return $ LBS.drop (fromIntegral $ length ver) src


readChunks :: LBS.ByteString -> [LBS.ByteString]
readChunks x
    | Just (n, x) <- grab 4 x
    , Just (y, x) <- grab (fromIntegral (decode n :: Word32)) x
    = y : readChunks x
    | otherwise = []
    where
        grab i x | LBS.length a == i = Just (a, b)
                 | otherwise = Nothing
            where (a,b) = LBS.splitAt i x


writeChunk :: Handle -> LBS.ByteString -> IO ()
writeChunk h x = do
    let n = encode (fromIntegral $ LBS.length x :: Word32)
    LBS.hPut h $ n `LBS.append` x
    hFlush h
