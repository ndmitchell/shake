{-# LANGUAGE ExistentialQuantification, RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, KindSignatures #-}

{- |
This module implements the Key/Value types, to abstract over hetrogenous data types.
-}
module Development.Shake.Internal.Value(
    Value, newValue, fromValue, typeValue,
    Key, newKey, fromKey, typeKey,
    currentWitness2, clearWitness, registerWitness,
    ShakeValue
    ) where

import General.Binary
import Development.Shake.Classes
import Development.Shake.Internal.Errors
import Data.Typeable.Extra

import Data.Bits
import Data.IORef
import Data.Maybe
import qualified Data.HashMap.Strict as Map
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
    deriving (Eq,Hashable,NFData)

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
newValue = Value (typeRep (Proxy :: Proxy a)) show (==) rnf hashWithSalt put

typeKey :: Key -> TypeRep
typeKey (Key v) = typeValue v

typeValue :: Value -> TypeRep
typeValue Value{..} = valueType

fromKey :: Typeable a => Key -> a
fromKey (Key v) = fromValue v

castValue :: forall a . Typeable a => Value -> Maybe a
castValue Value{..}
    | valueType == typeRep (Proxy :: Proxy a) = Just $ unsafeCoerce value
    | otherwise = Nothing

fromValue :: forall a . Typeable a => Value -> a
fromValue v = fromMaybe (err msg) $ castValue v
    where msg = "fromValue, bad cast, have " ++ show (valueType v) ++ ", wanted " ++ show (typeRep (Proxy :: Proxy a))

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

-- FIXME: These witnesses should be stored in the Rules type, not a global IORef
{-# NOINLINE witness2 #-}
witness2 :: IORef (Map.HashMap (TypeRep, TypeRep) (Key -> Put, Get Key, Value -> Put, Get Value))
witness2 = unsafePerformIO $ newIORef Map.empty

clearWitness :: IO ()
clearWitness = writeIORef witness2 Map.empty

registerWitness :: (ShakeValue k, ShakeValue v) => Proxy k -> Proxy v -> IO ()
registerWitness (k :: Proxy (k :: *)) (v :: Proxy (v :: *)) = atomicModifyIORef witness2 $ \mp -> (f mp, ())
    where f = Map.insert (typeRep k, typeRep v)
                (\k -> put (fromKey k :: k)
                ,do k <- get; return $ newKey (k :: k)
                ,\v -> put (fromValue v :: v)
                ,do v <- get; return $ newValue (v :: v))

currentWitness2 :: IO (Map.HashMap (TypeRep, TypeRep) (Key -> Put, Get Key, Value -> Put, Get Value))
currentWitness2 = readIORef witness2
