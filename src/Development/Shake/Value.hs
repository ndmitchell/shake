{-# LANGUAGE ExistentialQuantification, FlexibleInstances, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, ConstraintKinds #-}

{- |
This module implements the Key/Value types, to abstract over hetrogenous data types.
-}
module Development.Shake.Value(
    Value, newValue, fromValue,
    Key(..), newKey, fromKey, keyString,
    Witness, currentWitness, registerWitness,
    ShakeValue
    ) where

import Development.Shake.Classes
import Development.Shake.Errors
import Data.Typeable
import GHC.Generics
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Data.Dynamic
import General.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.IO.Unsafe

-- | Define an alias for the six type classes required for things involved in Shake 'Development.Shake.Rule's.
--   Using this alias requires the @ConstraintKinds@ extension.
--
--   To define your own values meeting the necessary constraints it is convenient to use the extensions
--   @GeneralizedNewtypeDeriving@ and @DeriveDataTypeable@ to write:
--
-- > newtype MyType = MyType (String, Bool) deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
--
--   Shake needs these instances on keys and values. They are used for:
--
-- * 'Show' is used to print out keys in errors, profiling, progress messages
--   and diagnostics.
--
-- * 'Typeable' is used because Shake indexes its database by the
--   type of the key and value involved in the rule (overlap is not
--   allowed for type classes and not allowed in Shake either).
--
-- * 'Eq' and 'Hashable' are used on keys in order to build hash maps
--   from keys to values.  'Eq' is used on values to test if the value
--   has changed or not (this is used to support unchanging rebuilds,
--   where Shake can avoid rerunning rules if it runs a dependency,
--   but it turns out that no changes occurred.)  The 'Hashable'
--   instances are only use at runtime (never serialised to disk),
--   so they do not have to be stable across runs.
--   Hashable on values is not used, and only required for a consistent interface.
--
-- * 'Binary' is used to serialize keys and values into Shake's
--   build database; this lets Shake cache values across runs and
--   implement unchanging rebuilds.
--
-- * 'NFData' is used to avoid space and thunk leaks, especially
--   when Shake is parallelized.
type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)

-- We deliberately avoid Typeable instances on Key/Value to stop them accidentally
-- being used inside themselves
data Key = Key TypeRep Value
    deriving (Typeable,Eq,Show,Hashable,NFData,Generic)

type Value = LBS.ByteString

newKey :: ShakeValue a => a -> IO Key
newKey a = Key (typeOf a) <$> newValue a

newValue :: ShakeValue a => a -> IO Value
newValue a = do
    ws <- currentWitness
    let msg = "no witness for " ++ show (typeOf a)
    return . runPut $ do
        put $ fromMaybe (error msg) $ Map.lookup (typeOf a) (witnessOut ws)
        put a

fromKey :: Typeable a => Key -> IO a
fromKey (Key _ v) = fromValue v

keyString :: Key -> LBS.ByteString
keyString (Key _ v) = v

fromValue :: Typeable a => Value -> IO a
fromValue x = do
    ws <- currentWitness
    return . flip runGet x $ do
        h <- get
        case Map.lookup h $ witnessIn ws of
            Nothing | h >= 0 && h < genericLength (typeNames ws) -> error $
                "Failed to find a type " ++ (typeNames ws !! fromIntegral h) ++ " which is stored in the database.\n" ++
                "The most likely cause is that your build tool has changed significantly."
            Nothing -> error $
                -- should not happen, unless proper data corruption
                "Corruption when reading Value, got type " ++ show h ++ ", but should be in range 0.." ++ show (length (typeNames ws) - 1)
            Just act -> fromMaybe (err "fromValue, bad cast") . fromDynamic <$> act

---------------------------------------------------------------------
-- BINARY INSTANCES

{-# NOINLINE witness #-}
witness :: IORef (Map.HashMap TypeRep (Get Dynamic))
witness = unsafePerformIO $ newIORef Map.empty

registerWitness :: ShakeValue a => a -> IO ()
registerWitness x = atomicModifyIORef witness $ \mp -> (Map.insert (typeOf x) (fmap toDyn (get `asTypeOf` return x)) mp, ())

-- Produce a list in a predictable order from a Map TypeRep, which should be consistent regardless of the order
-- elements were added and stable between program executions.
-- Cannot rely on hash (not pure in hashable-1.2) or compare (not available before 7.2)
toStableList :: Map.HashMap TypeRep v -> [(TypeRep,v)]
toStableList = sortBy (compare `on` show . fst) . Map.toList


data Witness = Witness
    {typeNames :: [String] -- the canonical data, the names of the types
    ,witnessIn :: Map.HashMap Word16 (Get Dynamic) -- for reading in, the find the values (some may be missing)
    ,witnessOut :: Map.HashMap TypeRep Word16 -- for writing out, find the value
    } deriving Show

instance Show (Get Dynamic) where
    show _ = "<Get Dynamic>"

instance Eq Witness where
    -- Type names are produced by toStableList so should to remain consistent
    -- regardless of the order of registerWitness calls.
    a == b = typeNames a == typeNames b

currentWitness :: IO Witness
currentWitness = do
    ws <- readIORef witness
    let (ks,vs) = unzip $ toStableList ws
    return $ Witness (map show ks) (Map.fromList $ zip [0..] vs) (Map.fromList $ zip ks [0..])

instance Binary Witness where
    put (Witness ts _ _) = put $ BS.unlines $ map BS.pack ts
    get = do
        ts <- fmap (map BS.unpack . BS.lines) get
        let ws = toStableList $ unsafePerformIO $ readIORefAfter ts witness
        let (is,ks,vs) = unzip3 [(i,k,v) | (i,t) <- zip [0..] ts, (k,v):_ <- [filter ((==) t . show . fst) ws]]
        return $ Witness ts (Map.fromList $ zip is vs) (Map.fromList $ zip ks is)
        where
            -- Read an IORef after examining a variable, used to avoid GHC over-optimisation
            {-# NOINLINE readIORefAfter #-}
            readIORefAfter :: a -> IORef b -> IO b
            readIORefAfter v ref = v `seq` readIORef ref

