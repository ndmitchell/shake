{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-
Files stores the meta-data so its very important its always accurate
We can't rely on getting a Ctrl+C at the end, so we'd better write out a journal
But if we do happen to get a Ctrl+C then that is very handy
The journal is idempotent, i.e. if we replay the journal twice all is good
-}

module Development.Shake.Database(
    Time, startTime, Duration, duration, Trace,
    Database, withDatabase,
    Ops(..), build,
    allEntries, showJSON, checkValid,
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
databaseVersion i = "SHAKE-DATABASE-3-" ++ show (i :: Int) ++ "\r\n"
journalVersion i = "SHAKE-JOURNAL-3-" ++ show (i :: Int) ++ "\r\n"


removeFile_ :: FilePath -> IO ()
removeFile_ x = catch (removeFile x) (\(e :: SomeException) -> return ())

type Map = Map.HashMap


---------------------------------------------------------------------
-- UTILITY TYPES

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
    ,journal :: Key -> Result -> IO ()
    ,logger :: String -> IO () -- logging function
    }

data Status
    = Ready Result -- I have a value
    | Error SomeException -- I have been run and raised an error
    | Loaded Result -- Loaded from the database
    | Waiting Pending (Maybe Result) -- Currently checking if I am valid or building
      deriving Show

data Result = Result
    {result :: Value -- the result associated with the Key
    ,built :: Step -- when it was actually run
    ,changed :: Step -- the step for deciding if it's valid
    ,depends :: [[Key]] -- dependencies
    ,execution :: Duration -- how long it took when it was last run (seconds)
    ,traces :: [Trace] -- a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving Show


newtype Pending = Pending (IORef (IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error or Ready

instance Show Pending where show _ = "Pending"


isError Error{} = True; isError _ = False
isWaiting Waiting{} = True; isWaiting _ = False
isReady Ready{} = True; isReady _ = False


-- All the waiting operations are only valid when isWaiting
type Waiting = Status

afterWaiting :: Waiting -> IO () -> IO ()
afterWaiting (Waiting (Pending p) _) act = modifyIORef p (>> act)

newWaiting :: Maybe Result -> IO Waiting
newWaiting r = do ref <- newIORef $ return (); return $ Waiting (Pending ref) r

runWaiting :: Waiting -> IO ()
runWaiting (Waiting (Pending p) _) = join $ readIORef p

-- Wait for a set of actions to complete
-- If the action returns True, the function will not be called again
-- If the first argument is True, the thing is ended
waitFor :: [(a, Waiting)] -> (Bool -> a -> IO Bool) -> IO ()
waitFor ws@(_:_) act = do
    todo <- newIORef $ length ws
    forM_ ws $ \(k,w) -> afterWaiting w $ do
        t <- readIORef todo
        when (t /= 0) $ do
            b <- act (t == 1) k
            writeIORef todo $ if b then 0 else t - 1


getResult :: Status -> Maybe Result
getResult (Ready r) = Just r
getResult (Loaded r) = Just r
getResult (Waiting _ r) = r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

data Ops = Ops
    {valid :: Key -> Value -> IO Bool
        -- ^ Given a Key and a Value from the database, check it still matches the value stored on disk
    ,exec :: [Key] -> Key -> IO (Either SomeException (Value, [[Key]], Duration, [Trace]))
        -- ^ Given a chunk of stack (bottom element first), and a key, either raise an exception or successfully build it
    }


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> Ops -> [Key] -> IO (Either SomeException (Duration,[Value]))
build pool Database{..} Ops{..} ks =
    join $ withLock lock $ do
        vs <- mapM (reduce []) ks
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ return $ Right (0, [result r | Ready r <- vs])
         else if not $ null errs then
            return $ return $ Left $ head errs
         else do
            wait <- newBarrier
            waitFor (filter (isWaiting . snd) $ zip ks vs) $ \finish k -> do
                s <- readIORef status
                let done x = do signalBarrier wait x; return True
                case Map.lookup k s of
                    Just (Error e) -> done $ Left e
                    Just Ready{} | finish -> done $ Right [result r | k <- ks, let Ready r = fromJust $ Map.lookup k s]
                                 | otherwise -> return False
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
            logger $ maybe "Missing" shw (Map.lookup k s) ++ " -> " ++ shw v ++ ", " ++ show k
            return v

        atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s

        -- Rules for each eval* function
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must not return Loaded

        reduce :: [Key] -> Key -> IO Status
        reduce stack k = do
            s <- readIORef status
            case Map.lookup k s of
                Nothing -> run stack k Nothing
                Just (Loaded r) -> do
                    b <- valid k $ result r
                    logger $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)
                    if not b then run stack k $ Just r else check stack k r (depends r)
                Just res -> return res

        run :: [Key] -> Key -> Maybe Result -> IO Waiting
        run stack k r = do
            w <- newWaiting r
            addPool pool $ do
                res <- exec stack k
                ans <- withLock lock $ do
                    ans <- k #= case res of
                        Left err -> Error err
                        Right (v,depends,execution,traces) ->
                            let c | Just r <- r, result r == v = changed r
                                  | otherwise = step
                            in Ready Result{result=v,changed=c,built=step,..}
                    runWaiting w
                    return ans
                case ans of
                    Ready r -> do
                        logger $ "result " ++ atom k ++ " = " ++ atom (result r)
                        journal k r -- leave the DB lock before appending
                    _ -> return ()
            k #= w

        check :: [Key] -> Key -> Result -> [[Key]] -> IO Status
        check stack k r [] =
            k #= Ready r
        check stack k r (ds:rest) = do
            vs <- mapM (reduce (k:stack)) ds
            let ws = filter (isWaiting . snd) $ zip ds vs
            if any isError vs || any (> built r) [changed | Ready Result{..} <- vs] then
                run stack k $ Just r
             else if null ws then
                check stack k r rest
             else do
                self <- newWaiting $ Just r
                waitFor ws $ \finish d -> do
                    s <- readIORef status
                    let buildIt = do
                            b <- run stack k $ Just r
                            afterWaiting b $ runWaiting self
                            return True
                    case Map.lookup d s of
                        Just Error{} -> buildIt
                        Just (Ready r2)
                            | changed r2 > built r -> buildIt
                            | finish -> do
                                res <- check stack k r rest
                                if not $ isWaiting res
                                    then runWaiting self
                                    else afterWaiting res $ runWaiting self
                                return True
                            | otherwise -> return False
                k #= self


---------------------------------------------------------------------
-- QUERY DATABASE

-- | Return a list of keys in an order which would build them bottom up. Relies on the invariant
--   that the database is not cyclic.
allEntries :: Database -> IO [(Key,Value)]
allEntries Database{..} = do
    status <- readIORef status
    return $ ordering [((k, result i), concat $ depends i) | (k,v) <- Map.toList status, Just i <- [getResult v]]
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


checkValid :: Database -> (Key -> Value -> IO Bool) -> IO ()
checkValid Database{..} valid = do
    status <- readIORef status
    logger "Starting validity/lint checking"
    bad <- fmap concat $ forM (Map.toList status) $ \(k,v) -> case v of
        Ready r -> do
            good <- valid k (result r)
            logger $ "Checking if " ++ show k ++ " is " ++ show (result r) ++ ", " ++ if good then "passed" else "FAILED"
            return [show k ++ " is no longer " ++ show (result r) | not good && not (special k)]
        _ -> return []
    if null bad
        then logger "Validity/lint check passed"
        else error $ unlines $ "Error: Dependencies have changed since being built:" : bad

    where
        -- special case for these things, since the purpose is to break the invariant
        special k = s == "AlwaysRun" || "Oracle " `isPrefixOf` s
            where s = show k

---------------------------------------------------------------------
-- DATABASE

data Storage = Storage
    {journal_ :: Journal Witness
    ,filename :: FilePath
    ,version :: Int -- user supplied version
    ,database :: Database
    }


withDatabase :: (String -> IO ()) -> FilePath -> Int -> (Database -> IO ()) -> IO ()
withDatabase logger filename version act = bracket
    (openDatabase logger filename version)
    (\s@Storage{database=Database{..}} -> do
        status <- readIORef status
        ws <- currentWitness
        closeDatabase logger s ws (step, Statuses status))
    (act . database)


-- Files are named based on the FilePath, but with different extensions,
-- such as .database, .journal, .trace
openDatabase :: (String -> IO ()) -> FilePath -> Int -> IO Storage
openDatabase logger filename version = do
    let dbfile = filename <.> "database"
        jfile = filename <.> "journal"
    createDirectoryIfMissing True $ takeDirectory dbfile

    lock <- newLock
    logger $ "readDatabase " ++ dbfile
    (step, Statuses status) <- fmap (fromMaybe (Step 1, Statuses $ Map.fromList [])) $
        readDatabase dbfile version (undefined :: Phantom Witness)
    step <- return $ incStep step

    b <- doesFileExist jfile
    (status,step) <- if not b then return (status,step) else do
        logger $ "replayJournal " ++ jfile
        extra <- replayJournal (undefined :: Phantom Witness) jfile version
        status <- return $ foldl' (\mp (k,v) -> Map.insert k (Loaded v) mp) status extra
        removeFile_ jfile
        -- the journal potentially things at the current step, so increment my step
        witness <- currentWitness
        writeDatabase dbfile version witness (step, Statuses status)
        logger $ "rewriteDatabase " ++ jfile
        return (status, incStep step)

    status <- newIORef status
    witness <- currentWitness
    journal_ <- openJournal jfile version witness
    let journal k r = appendJournal journal_ (k,r)
    logger "openDatabase complete"
    return Storage{database=Database{..},..}


closeDatabase :: (Binary w, BinaryWith w v) => (String -> IO ()) -> Storage -> w -> v -> IO ()
closeDatabase logger Storage{..} ws v = do
    let dbfile = filename <.> "database"
    logger $ "writeDatabase " ++ dbfile
    writeDatabase dbfile version ws v
    logger "closeJournal"
    closeJournal journal_
    logger "closeDatabase complete"


writeDatabase :: (Binary w, BinaryWith w v) => FilePath -> Int -> w -> v -> IO ()
writeDatabase file version ws v =
    LBS.writeFile file $
        LBS.pack (databaseVersion version) `LBS.append`
        encode (Witnessed ws v)


readDatabase :: forall w v . (Binary w, BinaryWith w v) => FilePath -> Int -> Phantom w -> IO (Maybe v)
readDatabase file version _ = do
    b <- doesFileExist file
    if not b
        then return Nothing
        else catch (do
            src <- readFileVer file $ databaseVersion version
            let Witnessed (_ :: w) a = decode src
            -- FIXME: The LBS.length shouldn't be necessary, but it is
            a `seq` LBS.length src `seq` return $ Just a) $
            \(err :: SomeException) -> do
                putStrLn $ unlines $
                    ("Error when reading Shake database " ++ file) :
                    map ("  "++) (lines $ show err) ++
                    ["All files will be rebuilt"]
                removeFile_ file -- so it doesn't error next time
                return Nothing


---------------------------------------------------------------------
-- JOURNAL

data Journal w = Journal
    {handle :: Var (Maybe Handle)
    ,journalFile :: FilePath
    ,witness :: w
    }

openJournal :: Binary w => FilePath -> Int -> w -> IO (Journal w)
openJournal journalFile ver witness = do
    h <- openBinaryFile journalFile WriteMode
    hSetFileSize h 0
    LBS.hPut h $ LBS.pack $ journalVersion ver
    writeChunk h $ encode witness
    hFlush h
    handle <- newVar $ Just h
    return Journal{..}

data Phantom w

replayJournal :: forall w u . (Binary w, BinaryWith w u) => Phantom w -> FilePath -> Int -> IO [u]
replayJournal _ file ver = catch (do
    src <- readFileVer file $ journalVersion ver
    let ws:rest = readChunks src
    ws :: w <- return $ decode ws
    return $ map (runGet (getWith ws)) rest)
    $ \(err :: SomeException) -> do
        putStrLn $ unlines $
            ("Error when reading Shake journal " ++ file) :
            map ("  "++) (lines $ show err) ++
            ["All files built in the last exceution will be rebuilt"]
        return []


appendJournal :: BinaryWith w u => Journal w -> u -> IO ()
appendJournal Journal{..} u = modifyVar_ handle $ \v -> case v of
    Nothing -> return Nothing
    Just h -> do
        writeChunk h $ runPut $ putWith witness u
        return $ Just h


closeJournal :: Journal w -> IO ()
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

instance BinaryWith Witness Step where
    putWith _ x = put x
    getWith _ = get


data Witnessed w a = Witnessed w a

instance (Binary w, BinaryWith w a) => Binary (Witnessed w a) where
    put (Witnessed ws x) = put ws >> putWith ws x
    get = do ws <- get; x <- getWith ws; return $ Witnessed ws x

-- Only for serialisation
newtype Statuses = Statuses (Map Key Status)

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
