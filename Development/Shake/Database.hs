{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS -fno-warn-unused-binds #-} -- for fields used just for docs
{-
Files stores the meta-data so its very important its always accurate
We can't rely on getting a Ctrl+C at the end, so we'd better write out a journal
But if we do happen to get a Ctrl+C then that is very handy
The journal is idempotent, i.e. if we replay the journal twice all is good
-}

module Development.Shake.Database(
    Database, withDatabase,
    request, Response(..), finished,
    allEntries, showJSON,
    ) where

import Development.Shake.Binary
import Development.Shake.Locks
import Development.Shake.Value

import Prelude hiding (catch)
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as S
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.List
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


removeFile_ :: FilePath -> IO ()
removeFile_ x = catch (removeFile x) (\(e :: SomeException) -> return ())


type Map = Map.HashMap

newtype Time = Time Int
    deriving (Eq,Ord,Show)

incTime (Time i) = Time $ i + 1



-- | Invariant: The database does not have any cycles when a Key depends on itself
data Database = Database
    {status :: Var (Map Key Status)
    ,timestamp :: Time
    ,journal :: Journal
    ,filename :: FilePath
    ,version :: Int -- user supplied version
    }

data Info = Info
    {value :: Value -- the value associated with the Key
    ,built :: Time -- the timestamp for deciding if it's valid
    ,changed :: Time -- when it was actually run
    ,depends :: [[Key]] -- dependencies
    ,execution :: Double -- how long it took when it was last run (seconds)
    ,traces :: [(String, Double, Double)] -- a trace of the expensive operations (start/end in seconds since beginning of run)
    }
    deriving Show


data Status
    = Building Barrier (Maybe Info)
    | Built  Info
    | Loaded Info
      deriving Show

getInfo :: Status -> Maybe Info
getInfo (Built i) = Just i
getInfo (Loaded i) = Just i
getInfo (Building _ i) = i


---------------------------------------------------------------------
-- OPERATIONS

data Response
    = Execute [Key] -- you need to execute these keys and call finished at least once before calling request again
    | Block ([Key], IO ()) -- you need to block on at least one of these barriers before calling request again
    | Response [Value] -- actual result values, do not call request again

data Response_ = Response_
    {execute :: [Key]
    ,barriers :: [(Key,Barrier)]
    ,values :: [(Time,Value)]
    }

concatResponse :: [Response_] -> Response_
concatResponse xs = Response_ (concatMap execute xs) (concatMap barriers xs) (concatMap values xs)

toResponse :: Response_ -> Response
toResponse Response_{..}
    | not $ null execute = Execute execute
    | not $ null barriers = Block $ second waitAnyBarrier $ unzip barriers
    | otherwise = Response $ map snd values


-- The idea behind request is that we do as much as we can with a single lock, which reduces a clean
-- rebuild to an absolute minimum traversal in single-threaded fast-path code. We may pay more repeatedly
-- calling 'request', but the depth of the call graph is low so it shouldn't be an issue, and with additional
-- complexity it could be avoided.
request :: Database -> (Key -> Value -> Bool) -> [Key] -> IO Response
request Database{..} validStored ks =
     modifyVar status $ \v -> do
        (res, mp) <- S.runStateT (fmap concatResponse $ mapM f ks) v
        return (mp, toResponse res)
    where
        f :: Key -> S.StateT (Map Key Status) IO Response_
        f k = do
            s <- S.get
            case Map.lookup k s of
                Nothing -> build k
                Just (Building bar _) -> return $ Response_ [] [(k,bar)] []
                Just (Built i) -> return $ Response_ [] [] [(changed i, value i)]
                Just (Loaded i) ->
                    if not $ validStored k (value i)
                    then build k
                    else validHistory k i (depends i)

        validHistory :: Key -> Info -> [[Key]] -> S.StateT (Map Key Status) IO Response_
        validHistory k i [] = do
            S.modify $ Map.insert k $ Built i
            return $ Response_ [] [] [(changed i, value i)]
        validHistory k i (x:xs) = do
            r@Response_{..} <- fmap concatResponse $ mapM f x
            if not $ null execute && null barriers then return r
             else if all ((<= built i) . fst) values then validHistory k i xs
             else build k

        build :: Key -> S.StateT (Map Key Status) IO Response_
        build k = do
            bar <- liftIO newBarrier
            S.modify $ \mp ->
                let info = case Map.lookup k mp of Nothing -> Nothing; Just (Loaded i) -> Just i
                in Map.insert k (Building bar info) mp
            return $ Response_ [k] [] []


finished :: Database -> Key -> Value -> [[Key]] -> Double -> [(String,Double,Double)] -> IO ()
finished Database{..} k v depends duration traces = do
    let info = Info v timestamp timestamp depends duration traces
    (info2, barrier) <- modifyVar status $ \mp -> return $
        let Just (Building bar old) = Map.lookup k mp
            info2 = if isJust old && value (fromJust old) == value info then info{changed=changed $ fromJust old} else info
        in (Map.insert k (Built info2) mp, (info2, bar))
    appendJournal journal k info2
    releaseBarrier barrier


-- | Return a list of keys in an order which would build them bottom up. Relies on the invariant
--   that the database is not cyclic.
allEntries :: Database -> IO [(Key,Value)]
allEntries Database{..} = do
    status <- readVar status
    return $ ordering [((k, value i), concat $ depends i) | (k,v) <- Map.toList status, Just i <- [getInfo v]]
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
    status <- readVar status
    let ids = Map.fromList $ zip (Map.keys status) [0..]
        f (k, v) | Just Info{..} <- getInfo v =
            let xs = ["name:" ++ show (show k)
                     ,"built:" ++ showTime built
                     ,"changed:" ++ showTime changed
                     ,"depends:" ++ show (mapMaybe (`Map.lookup` ids) (concat depends))
                     ,"execution:" ++ show execution] ++
                     ["traces:" ++ show traces | traces /= []]
                showTime (Time i) = show i
            in  ["{" ++ intercalate ", " xs ++ "}"]
        f _ = []
    return $ "[" ++ intercalate "\n," (concatMap f $ Map.toList status) ++ "\n]"


---------------------------------------------------------------------
-- DATABASE

withDatabase :: FilePath -> Int -> (Database -> IO a) -> IO a
withDatabase filename version = bracket (openDatabase filename version) closeDatabase


-- Files are named based on the FilePath, but with different extensions,
-- such as .database, .journal, .trace
openDatabase :: FilePath -> Int -> IO Database
openDatabase filename version = do
    let dbfile = filename <.> "database"
        jfile = filename <.> "journal"

    (timestamp, status) <- readDatabase dbfile version
    timestamp <- return $ incTime timestamp
    
    b <- doesFileExist jfile
    (status,timestamp) <- if not b then return (status,timestamp) else do
        status <- replayJournal jfile version status
        removeFile_ jfile
        -- the journal potentially things at the current timestamp, so increment my timestamp
        writeDatabase dbfile version timestamp status
        return (status, incTime timestamp)

    status <- newVar status
    journal <- openJournal jfile version
    return Database{..}


closeDatabase :: Database -> IO ()
closeDatabase Database{..} = do
    status <- readVar status
    writeDatabase (filename <.> "database") version timestamp status
    closeJournal journal


writeDatabase :: FilePath -> Int -> Time -> Map Key Status -> IO ()
writeDatabase file version timestamp status = do
    ws <- currentWitness
    LBS.writeFile file $
        (LBS.pack $ databaseVersion version) `LBS.append`
        encode (timestamp, Witnessed ws $ Statuses status)


readDatabase :: FilePath -> Int -> IO (Time, Map Key Status)
readDatabase file version = do
    let zero = (Time 1, Map.fromList [])
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


appendJournal :: Journal -> Key -> Info -> IO ()
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

instance Binary Time where
    put (Time i) = put i
    get = fmap Time get


data Witnessed a = Witnessed Witness a
fromWitnessed (Witnessed _ x) = x

instance BinaryWith Witness a => Binary (Witnessed a) where
    put (Witnessed ws x) = put ws >> putWith ws x
    get = do ws <- get; x <- getWith ws; return $ Witnessed ws x

-- Only for serialisation
newtype Statuses = Statuses {fromStatuses :: Map Key Status}

instance BinaryWith Witness Statuses where
    putWith ws (Statuses x) = putWith ws [(k,i) | (k,v) <- Map.toList x, Just i <- [f v]]
        where
            f (Building _ i) = i
            f (Built i) = Just i
            f (Loaded i) = Just i
    getWith ws = do
        x <- getWith ws
        return $ Statuses $ Map.fromList $ map (second Loaded) x

instance BinaryWith Witness Info where
    putWith ws (Info x1 x2 x3 x4 x5 x6) = putWith ws x1 >> put x2 >> put x3 >> putWith ws x4 >> put x5 >> put x6
    getWith ws = do x1 <- getWith ws; x2 <- get; x3 <- get; x4 <- getWith ws; x5 <- get; x6 <- get; return $ Info x1 x2 x3 x4 x5 x6


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
