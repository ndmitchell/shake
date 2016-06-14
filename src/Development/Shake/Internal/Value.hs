{-# LANGUAGE ExistentialQuantification, RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}

{- |
This module implements the Key/Value types, to abstract over hetrogenous data types.
-}
module Development.Shake.Internal.Value(
    Value, newValue, fromValue, typeValue,
    Key, newKey, fromKey, typeKey,
    Witness, currentWitness, registerWitness,
    ShakeValue
    ) where

import General.Binary
import Development.Shake.Classes
import Development.Shake.Internal.Errors
import Data.Typeable

import Data.Bits
import Data.IORef
import Data.List.Extra
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe
import Unsafe.Coerce

-- | Define an alias for the six type classes required for things involved in Shake rules.
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
newtype Key = Key Value
    deriving (Eq,Hashable,NFData,BinaryWith Witness)

data Value = forall a . Value
    {valueType :: TypeRep
    ,valueShow :: a -> String
    ,valueEq :: a -> a -> Bool
    ,valueRnf :: a -> ()
    ,valueHash :: Int -> a -> Int
    ,valuePut :: a -> Put
    ,value :: a
    }


newKey :: ShakeValue a => a -> Key
newKey = Key . newValue

newValue :: forall a . ShakeValue a => a -> Value
newValue = Value (typeOf (undefined :: a)) show (==) rnf hashWithSalt put

typeKey :: Key -> TypeRep
typeKey (Key v) = typeValue v

typeValue :: Value -> TypeRep
typeValue Value{..} = valueType

fromKey :: Typeable a => Key -> a
fromKey (Key v) = fromValue v

castValue :: forall a . Typeable a => Value -> Maybe a
castValue Value{..}
    | valueType == typeOf (undefined :: a) = Just $ unsafeCoerce value
    | otherwise = Nothing

fromValue :: Typeable a => Value -> a
fromValue = fromMaybe (err "fromValue, bad cast") . castValue

instance Show Key where
    show (Key a) = show a

instance Show Value where
    show Value{..} = valueShow value

instance NFData Value where
    rnf Value{..} = valueRnf value

instance Hashable Value where
    hashWithSalt salt Value{..} = hashWithSalt salt valueType `xor` valueHash salt value

instance Eq Value where
    Value{valueType=at,value=a,valueEq=eq} == Value{valueType=bt,value=b}
        | at /= bt = False
        | otherwise = eq a (unsafeCoerce b)


---------------------------------------------------------------------
-- BINARY INSTANCES

{-# NOINLINE witness #-}
witness :: IORef (Map.HashMap TypeRep (Get Value))
witness = unsafePerformIO $ newIORef Map.empty

registerWitness :: (ShakeValue k, ShakeValue v) => k -> v -> IO ()
registerWitness k v = atomicModifyIORef witness $ \mp -> (f k $ f v mp, ())
    where f x = Map.insert (typeOf x) (do v <- get; return $ newValue $ v `asTypeOf` x)


-- Produce a list in a predictable order from a Map TypeRep, which should be consistent regardless of the order
-- elements were added and stable between program executions.
-- Don't rely on the hashmap order since that might not be stable, if hashes clash.
toStableList :: Ord k => Map.HashMap k v -> [(k,v)]
toStableList = sortOn fst . Map.toList


data Witness = Witness
    {typeNames :: [String] -- the canonical data, the names of the types
    ,witnessIn :: Map.HashMap Word16 (Get Value) -- for reading in, the find the values (some may be missing)
    ,witnessOut :: Map.HashMap TypeRep Word16 -- for writing out, find the value
    }

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


instance BinaryWith Witness Value where
    putWith ws Value{..} = do
        let msg = "no witness for " ++ show valueType
        put $ fromMaybe (error msg) $ Map.lookup valueType (witnessOut ws)
        valuePut value

    getWith ws = do
        h <- get
        case Map.lookup h $ witnessIn ws of
            Nothing | h >= 0 && h < genericLength (typeNames ws) -> error $
                "Failed to find a type " ++ (typeNames ws !! fromIntegral h) ++ " which is stored in the database.\n" ++
                "The most likely cause is that your build tool has changed significantly."
            Nothing -> error $
                -- should not happen, unless proper data corruption
                "Corruption when reading Value, got type " ++ show h ++ ", but should be in range 0.." ++ show (length (typeNames ws) - 1)
            Just get -> get
