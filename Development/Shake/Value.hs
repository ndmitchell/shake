{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

{- |
This module implements the Key/Value types, to abstract over hetrogenous data types.
-}
module Development.Shake.Value(
    Value, newValue, fromValue, typeValue,
    Key, newKey, fromKey, typeKey,
    registerWitness
    ) where

import Data.Binary
import Data.Hashable
import Data.Typeable

import Data.Bits
import Data.IORef
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Development.Shake.TypeHash()
import System.IO.Unsafe


-- We deliberately avoid Typeable instances on Key/Value to stop them accidentally
-- being used inside themselves
newtype Key = Key Value
    deriving (Eq,Hashable,Binary)

data Value = forall a . (Eq a, Show a, Typeable a, Hashable a, Binary a) => Value a


newKey :: (Eq a, Show a, Typeable a, Hashable a, Binary a) => a -> Key
newKey = Key . newValue

newValue :: (Eq a, Show a, Typeable a, Hashable a, Binary a) => a -> Value
newValue = Value

typeKey :: Key -> TypeRep
typeKey (Key v) = typeValue v

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromKey :: Typeable a => Key -> a
fromKey (Key v) = fromValue v

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error msg) $ cast x
    where msg = "Internal error in Shake.fromValue, bad cast"

instance Show Key where
    show (Key a) = show a

instance Show Value where
    show (Value a) = show a

instance Hashable Value where
    hash (Value a) = hash (typeOf a) `xor` hash a

instance Eq Value where
    Value a == Value b = case cast b of
        Just bb -> a == bb
        Nothing -> False

instance Binary Value where
    put (Value x) = do
        put (hash $ typeOf x)
        put x

    get = do
        h <- get
        Value t <- return $ unsafePerformIO $ findWitness h
        x <- get
        return $ Value $ x `asTypeOf` t


{-# NOINLINE witnesses #-}
witnesses :: IORef (Map.HashMap Int Value)
witnesses = unsafePerformIO $ newIORef Map.empty

registerWitness :: (Eq a, Show a, Typeable a, Hashable a, Binary a) => a -> IO ()
registerWitness x = modifyIORef witnesses $ Map.insert (hash $ typeOf x) (Value x)

findWitness :: Int -> IO Value
findWitness i = do
    ws <- readIORef witnesses
    let err = "Failed to find witness for a type with hash " ++ show i ++ ". The most likely cause " ++
              "is that your build tool has changed significantly. A wipe/clean should fix the problem."
    return $ fromMaybe (error err) $ Map.lookup i ws
