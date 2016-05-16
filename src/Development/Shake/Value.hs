{-# LANGUAGE ExistentialQuantification, FlexibleInstances, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, ConstraintKinds #-}

{- |
This module implements the Key/Value types, to abstract over hetrogenous data types.
-}
module Development.Shake.Value(
    Value,  Key(..), newKey, fromKey, fromKeyDef,
    Witness, currentWitness, registerWitness,
    putType, putKeyWith, getType, getKeyWith,
    ShakeValue
    ) where

import Development.Shake.Classes
import Data.Typeable
import GHC.Generics
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import General.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.HashSet as Set
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
--   Shake needs these instances on keys. They are used for:
--
-- * 'Show' is used to print out keys in errors, profiling, progress messages
--   and diagnostics.
--
-- * 'Typeable' is used because Shake indexes its database by the
--   type of the key involved in the rule (overlap is not
--   allowed for type classes and not allowed in Shake either);
--   this lets Shake implement built-in rules
--
-- * 'Eq' and 'Hashable' are used on keys in order to build hash maps
--   from keys to values. The 'Hashable' instances are only use at
--   runtime (never serialised to disk),
--   so they do not have to be stable across runs.
--
-- * 'Binary' is used to serialize keys to and from Shake's build database
--
-- * 'NFData' is used to avoid space and thunk leaks, especially
--   when Shake is parallelized.
type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)

type Value = LBS.ByteString

data Key = Key
  { typeKey :: TypeRep
  , keyString :: Value
  }
    deriving (Typeable,Eq,Show,Hashable,NFData,Generic)

putType :: Witness -> TypeRep -> Put
putType ws t = do
    let msg = "no witness for " ++ show t
    put $ fromMaybe (error msg) $ Map.lookup t (witnessOut ws)

getType :: Witness -> Get TypeRep
getType ws = do
    h <- get
    case Map.lookup h $ witnessIn ws of
        Nothing | h >= 0 && h < genericLength (typeNames ws) -> error $
            "Failed to find a type " ++ (typeNames ws !! fromIntegral h) ++ " which is stored in the database.\n" ++
            "The most likely cause is that your build tool has changed significantly."
        Nothing -> error $
            -- should not happen, unless proper data corruption
            "Corruption when reading Value, got type " ++ show h ++ ", but should be in range 0.." ++ show (length (typeNames ws) - 1)
        Just ty -> return ty

putKeyWith :: Witness -> Key -> Put
putKeyWith ws (Key t v) = do
    putType ws t
    putLazyByteString v

getKeyWith :: Witness -> Get Key
getKeyWith ws = do
    ty <- getType ws
    Key ty <$> getRemainingLazyByteString

newKey :: (Typeable a, Binary a) => a -> Key
newKey a = Key (typeOf a) (encode a)

fromKey :: (Typeable a, Binary a) => Key -> Maybe a
fromKey (Key t v) = case decode v of
    r | t == typeOf r -> Just r
      | otherwise     -> Nothing

fromKeyDef :: (Typeable a, Binary a) => Key -> a -> a
fromKeyDef (Key t v) def = case decode v of
    r | t == typeOf r -> r
      | otherwise     -> def

---------------------------------------------------------------------
-- BINARY INSTANCES

{-# NOINLINE witness #-}
witness :: IORef (Set.HashSet TypeRep)
witness = unsafePerformIO $ newIORef Set.empty

registerWitness :: (Typeable a) => a -> IO ()
registerWitness x = registerWitness' (typeOf x)

registerWitness' :: TypeRep -> IO ()
registerWitness' x = atomicModifyIORef witness $ \mp -> (Set.insert x mp, ())

-- Produce a list in a predictable order from a Map TypeRep, which should be consistent regardless of the order
-- elements were added and stable between program executions.
-- Cannot rely on hash (not pure in hashable-1.2) or compare (not available before 7.2)
toStableList :: Set.HashSet TypeRep -> [TypeRep]
toStableList = sortBy (compare `on` show) . Set.toList

data Witness = Witness
    {typeNames :: [String] -- the canonical data, the names of the types
    ,witnessIn :: Map.HashMap Word16 TypeRep -- for reading in, find the values (some may be missing)
    ,witnessOut :: Map.HashMap TypeRep Word16 -- for writing out, find the value
    } deriving Show

instance Eq Witness where
    -- Type names are produced by toStableList so should to remain consistent
    -- regardless of the order of registerWitness calls.
    a == b = typeNames a == typeNames b

currentWitness :: IO Witness
currentWitness = do
    ws <- readIORef witness
    let ks = toStableList ws
    return $ Witness (map show ks) (Map.fromList $ zip [0..] ks) (Map.fromList $ zip ks [0..])

instance Binary Witness where
    put (Witness ts _ _) = put $ BS.unlines $ map BS.pack ts
    get = do
        ts <- fmap (map BS.unpack . BS.lines) get
        let ws = toStableList $ unsafePerformIO $ readIORefAfter ts witness
        let ks = [ k | t <- ts, let k = head $ filter ((==) t . show) ws]
        return $ Witness ts (Map.fromList $ zip [0..] ks) (Map.fromList $ zip ks [0..])
        where
            -- Read an IORef after examining a variable, used to avoid GHC over-optimisation
            {-# NOINLINE readIORefAfter #-}
            readIORefAfter :: a -> IORef b -> IO b
            readIORefAfter v ref = v `seq` readIORef ref

