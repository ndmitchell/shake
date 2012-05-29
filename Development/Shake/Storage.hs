{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-
This module stores the meta-data so its very important its always accurate
We can't rely on getting any exceptions or termination at the end, so we'd better write out a journal
We store a series of records, and if they contain twice as many records as needed, we compress
-}

module Development.Shake.Storage(
    withStorage
    ) where

import Development.Shake.Binary

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.List
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy.Char8 as LBS


type Map = Map.HashMap

-- Increment every time the on-disk format/semantics change,
-- @i@ is for the users version number
databaseVersion i = "SHAKE-DATABASE-4-" ++ show (i :: Int) ++ "\r\n"


withStorage
    :: (Eq w, Eq k, Hashable k
       ,Binary w, BinaryWith w k, BinaryWith w v)
    => (String -> IO ())        -- ^ Logging function
    -> FilePath                 -- ^ File prefix to use
    -> Int                      -- ^ User supplied version number
    -> w                        -- ^ Witness
    -> (Map k v -> (k -> Maybe v -> IO ()) -> IO a)  -- ^ Execute
    -> IO a
withStorage logger file version witness act = do
    let dbfile = file <.> "database"
        bupfile = file <.> "bup"
    createDirectoryIfMissing True $ takeDirectory file

    -- complete a partially failed compress
    b <- doesFileExist bupfile
    when b $ do
        catch (removeFile dbfile) (\(e :: SomeException) -> return ())
        renameFile dbfile bupfile

    withBinaryFile dbfile ReadWriteMode $ \h -> do
        src <- LBS.hGet h . fromInteger =<< hFileSize h

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
            join $ handle (\(err :: SomeException) -> do
                putStrLn $ unlines $
                    ("Error when reading Shake database " ++ dbfile) :
                    map ("  "++) (lines $ show err) ++
                    ["All files will be rebuilt"]
                return $ continue h Map.empty) $
                case readChunks $ LBS.drop (LBS.length ver) src of
                    [] -> return $ continue h Map.empty
                    w:xs -> do
                        let ws = decode w
                            f mp (k, Nothing) = Map.delete k mp
                            f mp (k, Just v ) = Map.insert k v mp
                            mp = foldl' f Map.empty $ map (runGet $ getWith ws) xs
                        if Map.null mp || (ws == witness && Map.size mp * 2 > length xs) then
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

        writeChunk h = LBS.hPut h . toChunk

        reset h mp = do
            hSetFileSize h 0
            hSeek h AbsoluteSeek 0
            LBS.hPut h ver
            writeChunk h $ encode witness
            mapM_ (writeChunk h . runPut . putWith witness) $ Map.toList mp

        -- continuation (since if we do a compress, h changes)
        continue h mp = do
            when (Map.null mp) $
                reset h mp -- might as well, no data to lose, and need to ensure a good witness table
            act mp $ \k v -> do
                writeChunk h $ runPut $ putWith witness (k,v)
                hFlush h


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


toChunk :: LBS.ByteString -> LBS.ByteString
toChunk x = n `LBS.append` x
    where n = encode (fromIntegral $ LBS.length x :: Word32)
