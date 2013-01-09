{-# LANGUAGE ScopedTypeVariables, PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}
{-
This module stores the meta-data so its very important its always accurate
We can't rely on getting any exceptions or termination at the end, so we'd better write out a journal
We store a series of records, and if they contain twice as many records as needed, we compress
-}

module Development.Shake.Storage(
    withStorage
    ) where

import Development.Shake.Binary
import Development.Shake.Locks

import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Concurrent
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Development.Shake.Classes
import qualified Data.HashMap.Strict as Map
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy.Char8 as LBS


type Map = Map.HashMap

-- Increment every time the on-disk format/semantics change,
-- @i@ is for the users version number
databaseVersion i = "SHAKE-DATABASE-6-" ++ show (i :: Int) ++ "\r\n"


withStorage
    :: (Eq w, Eq k, Hashable k
       ,Binary w, BinaryWith w k, BinaryWith w v)
    => (String -> IO ())        -- ^ Logging function
    -> FilePath                 -- ^ File prefix to use
    -> Int                      -- ^ User supplied version number
    -> Maybe Double             -- ^ How often to flush (Nothing = never, Just = seconds)
    -> w                        -- ^ Witness
    -> (Map k v -> (k -> v -> IO ()) -> IO a)  -- ^ Execute
    -> IO a
withStorage logger file version flush witness act = do
    let dbfile = file <.> "database"
        bupfile = file <.> "bup"
    createDirectoryIfMissing True $ takeDirectory file

    -- complete a partially failed compress
    b <- doesFileExist bupfile
    when b $ do
        logger $ "Backup file move to original"
        E.catch (removeFile dbfile) (\(e :: SomeException) -> return ())
        renameFile bupfile dbfile

    withBinaryFile dbfile ReadWriteMode $ \h -> do
        n <- hFileSize h
        logger $ "Reading file of size " ++ show n
        src <- LBS.hGet h $ fromInteger n

        if not $ ver `LBS.isPrefixOf` src then do
            unless (LBS.null src) $ do
                let good x = isAlphaNum x || x `elem` "-_ "
                let bad = LBS.takeWhile good $ LBS.take 50 src
                putStr $ unlines
                    ["Error when reading Shake database " ++ dbfile
                    ,"  Invalid version stamp detected"
                    ,"  Expected: " ++ takeWhile good (LBS.unpack ver)
                    ,"  Found   : " ++ LBS.unpack bad
                    ,"All files will be rebuilt"]
            continue h Map.empty
         else
            -- make sure you are not handling exceptions from inside
            join $ handleJust (\e -> if asyncException e then Nothing else Just e) (\err -> do
                msg <- showException err
                putStrLn $ unlines $
                    ("Error when reading Shake database " ++ dbfile) :
                    map ("  "++) (lines msg) ++
                    ["All files will be rebuilt"]
                -- exitFailure -- should never happen without external corruption
                               -- add back to check during random testing
                return $ continue h Map.empty) $
                case readChunks $ LBS.drop (LBS.length ver) src of
                    (slop, []) -> do
                        logger $ "Read 0 chunks, plus " ++ show slop ++ " slop"
                        return $ continue h Map.empty
                    (slop, w:xs) -> do
                        logger $ "Read " ++ show (length xs + 1) ++ " chunks, plus " ++ show slop ++ " slop"
                        logger $ "Chunk sizes " ++ show (map LBS.length (w:xs))
                        let ws = decode w
                            f mp (k, v) = Map.insert k v mp
                            mp = foldl' f Map.empty $ map (runGet $ getWith ws) xs
                        -- if mp is null, continue will reset it, so no need to clean up
                        if Map.null mp || (ws == witness && Map.size mp * 2 > length xs - 2) then do
                            -- make sure we reset to before the slop
                            when (not (Map.null mp) && slop /= 0) $ do
                                logger $ "Dropping last " ++ show slop ++ " bytes of database (incomplete)"
                                now <- hFileSize h
                                hSetFileSize h $ now - slop
                                hSeek h AbsoluteSeek $ now - slop
                                hFlush h
                                logger $ "Drop complete"
                            return $ continue h mp
                         else do
                            logger "Compressing database"
                            hClose h -- two hClose are fine
                            return $ do
                                renameFile dbfile bupfile
                                withBinaryFile dbfile ReadWriteMode $ \h -> do
                                    reset h mp
                                    removeFile bupfile
                                    logger "Compression complete"
                                    continue h mp
    where
        ver = LBS.pack $ databaseVersion version

        writeChunk h s = do
            logger $ "Writing chunk " ++ show (LBS.length s)
            LBS.hPut h $ toChunk s

        reset h mp = do
            logger $ "Resetting database to " ++ show (Map.size mp) ++ " elements"
            hSetFileSize h 0
            hSeek h AbsoluteSeek 0
            LBS.hPut h ver
            writeChunk h $ encode witness
            mapM_ (writeChunk h . runPut . putWith witness) $ Map.toList mp
            hFlush h
            logger "Flush"

        -- continuation (since if we do a compress, h changes)
        continue h mp = do
            when (Map.null mp) $
                reset h mp -- might as well, no data to lose, and need to ensure a good witness table
            lock <- newLock
            flushThread flush h $ \out ->
                act mp $ \k v -> out $ toChunk $ runPut $ putWith witness (k, v)


-- We avoid calling flush too often on SSD drives, as that can be slow
-- Do not move writes to a separate thread, as then we'd have to marshal exceptions back which is tricky
flushThread :: Maybe Double -> Handle -> ((LBS.ByteString -> IO ()) -> IO a) -> IO a
flushThread flush h act = do
    alive <- newVar True
    kick <- newEmptyMVar

    case flush of
        Nothing -> return ()
        Just flush -> do
            let delay = ceiling $ flush * 1000000
            let loop = do
                    takeMVar kick
                    threadDelay delay
                    b <- withVar alive $ \b -> do
                        when b $ do
                            tryTakeMVar kick
                            hFlush h
                        return b
                    when b loop
            forkIO $ do
                let msg = "Warning: Flushing Shake journal failed, on abnormal termination you may lose some data, "
                (loop >> return ()) `E.catch` \(e :: SomeException) -> putStrLn $ msg ++ show e
            return ()

    lock <- newLock
    (act $ \s -> do
            withLock lock $ LBS.hPut h s
            tryPutMVar kick ()
            return ())
        `finally` do
            tryPutMVar kick ()
            modifyVar_ alive $ const $ return False


-- Return the amount of junk at the end, along with all the chunk
readChunks :: LBS.ByteString -> (Integer, [LBS.ByteString])
readChunks x
    | Just (n, x) <- grab 4 x
    , Just (y, x) <- grab (fromIntegral (decode n :: Word32)) x
    = second (y :) $ readChunks x
    | otherwise = (toInteger $ LBS.length x, [])
    where
        grab i x | LBS.length a == i = Just (a, b)
                 | otherwise = Nothing
            where (a,b) = LBS.splitAt i x


toChunk :: LBS.ByteString -> LBS.ByteString
toChunk x = n `LBS.append` x
    where n = encode (fromIntegral $ LBS.length x :: Word32)


-- Some exceptions may have an error message which is itself an exception,
-- make sure you show them properly
showException :: SomeException -> IO String
showException err = do
    let msg = show err
    E.catch (evaluate $ rnf msg `seq` msg) (\(_ :: SomeException) -> return "Unknown exception (error while showing error message)")


-- | Is the exception asyncronous, not a "coding error" that should be ignored
asyncException :: SomeException -> Bool
asyncException e
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False
