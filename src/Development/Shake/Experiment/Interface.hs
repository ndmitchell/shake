{-# LANGUAGE Rank2Types, DeriveFunctor, TupleSections, ScopedTypeVariables, FlexibleInstances #-}
{-# ANN module "HLint: ignore" #-}

-- | Proposed new interface for defining custom rules. There are two types of rules:
--
--   * Builtin rules map between keys/values and must be unique for any key type.
--     A builtin rule declares how to build, when to rebuild and what to store.
--     As an example, file rules are implemented by a single builtin rule.
--
--   * User rules are just arbitrary values which are accumulated in the Rules structure.
--     There may be any number of rules per type. The builtin rules are responsible for
--     interogating and applying the user rules. Each %> call will result in one user rule.
--
--   The builtin rule interface is meant to be deliberately low-level. The idea is to allow anything that
--   makes sense to be implemented efficiently and correctly.
--   It is expected most people will use higher-level facilities such as 'addOracle' or other
--   appropriate sugar.
--
--   The user rule interface provides no type safety, and it is expected that sugared type-safe
--   methods will be defined on top.
module Development.Shake.Experiment.Interface(
    Encoder(..), encodeStorable, decodeStorable, encodeStorableList, decodeStorableList,
    addHelpTarget, addUserRule,
    UserRule(..), userRuleMatch,
    BuiltinRule, BuiltinInfo(..), addBuiltinRule
    ) where

import Development.Shake.Classes
import Development.Shake.Types
import Development.Shake.Core
import Data.Proxy
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Foreign.Storable
import Data.Tuple.Extra
import Foreign.Ptr
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

-- forM for zipWith
for2M_ as bs f = zipWithM_ f as bs


---------------------------------------------------------------------
-- BINARY SERIALISATION

-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--   Optimised for performance.
class Encoder a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> a

instance Encoder BS.ByteString where
    encode = id
    decode = id

instance Encoder [BS.ByteString] where
    -- Format:
    -- n :: Word32 - number of strings
    -- ns :: [Word32]{n} - length of each string
    -- contents of each string concatenated (sum ns bytes)
    encode xs = BS.unsafeCreate (4 + (n * 4) + sum ns) $ \p -> do
        pokeByteOff p 0 (fromIntegral n :: Word32)
        for2M_ [4,8..] ns $ \i x -> pokeByteOff p i (fromIntegral x :: Word32)
        p <- return $ p `plusPtr` (4 + (n * 4))
        for2M_ (scanl (+) 0 ns) xs $ \i x -> BS.useAsCStringLen x $ \(bs, n) ->
            BS.memcpy (castPtr bs) (p `plusPtr` i) n
        where ns = map BS.length xs
              n = length ns

    decode bs = unsafePerformIO $ BS.useAsCString bs $ \p -> do
        n <- fromIntegral <$> (peekByteOff p 0 :: IO Word32)
        ns :: [Word32] <- forM [1..fromIntegral n] $ \i -> peekByteOff p (i * 4)
        return $ snd $ mapAccumL (\bs i -> swap $ BS.splitAt (fromIntegral i) bs) (BS.drop (4 + (n * 4)) bs) ns

instance Encoder () where
    encode () = BS.empty
    decode _ = ()

instance Encoder Bool where
    encode False = bsFalse
    encode True = BS.empty
    decode = BS.null

-- CAF so the True ByteString is shared
bsFalse = BS.singleton 0

instance Encoder Word32 where
    encode = encodeStorable
    decode = decodeStorable


encodeStorable :: forall a . Storable a => a -> BS.ByteString
encodeStorable = \x -> BS.unsafeCreate n $ \p -> poke (castPtr p) x
    where n = sizeOf (undefined :: a)

decodeStorable :: forall a . Storable a => BS.ByteString -> a
decodeStorable = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
        if size /= n then error "size mismatch" else peek (castPtr p)
    where n = sizeOf (undefined :: a)


encodeStorableList :: forall a . Storable a => [a] -> BS.ByteString
encodeStorableList = \xs -> BS.unsafeCreate (n * length xs) $ \p ->
    for2M_ [0,n..] xs $ \i x -> pokeByteOff p i x
    where n = sizeOf (undefined :: a)

decodeStorableList :: forall a . Storable a => BS.ByteString -> [a]
decodeStorableList = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
    let (d,m) = size `divMod` n in
    if m /= 0 then error "size mismatch" else forM [0..d-1] $ \i -> peekElemOff (castPtr p) d
    where n = sizeOf (undefined :: a)


---------------------------------------------------------------------
-- USER RULES

-- | Add a line to the @--help@ output defining a target that can be built.
addHelpTarget :: String -> Rules ()
addHelpTarget = undefined

-- | Add an arbitrary user rule, most people will call sugared variants of this function.
addUserRule :: Typeable a => a -> Rules ()
addUserRule = undefined

-- | A 'Match' data type, representing calls to 'addUserRule', 'priority' and 'alternatives'.
--
--   /Semantics/
--
-- > priority p1 (priority p2 x) == priority p1 x
-- > priority p (x `ordered` y) = priority p x `ordered` priority p y
-- > priority p (x `unordered` y) = priority p x `unordered` priority p y
-- > ordered is associative
-- > unordered is associative and commutative
-- > alternative does not obey priorities, until picking the best one
data UserRule a
    = UserRule a -- ^ Added to the state with @'addUserRule' :: Typeable a => a -> Rules ()@.
    | Priority Double (UserRule a) -- ^ Rules defined under 'priority'.
    | Unordered [UserRule a] -- ^ Rules defined normally, at most one should match.
    | Ordered [UserRule a] -- ^ Rules defined under 'alternative', matched in order.
      deriving (Eq,Show,Functor)


-- | Given a set of rules, and a function that declares if a rule matches, produce the rules that match.
--   The resulting list is unordered - typically anything other than a singleton list will result in an error.
userRuleMatch :: UserRule a -> (a -> Maybe b) -> [b]
userRuleMatch u test = maybe [] snd $ f Nothing u
    where
        f p (UserRule u) = (fromMaybe 1 p,) . return <$> test u
        f p (Priority p2 x) = f (Just $ fromMaybe p2 p) x
        f p (Ordered xs) = listToMaybe $ mapMaybe (f p) xs
        f p (Unordered xs) = if null ys then Nothing else Just (mx, concatMap snd ys)
            where ys = mapMaybe (f p) xs
                  mx = maximum (map fst ys)


---------------------------------------------------------------------
-- DEFINING RULES

-- | Add a new builtin rule, which maps between keys and values.
--   For every distinct key there may be at most one rule defined.
addBuiltinRule
    :: (Encoder key, Hashable key, Eq key, Typeable key, Typeable value)
    => BuiltinRule key value -> Rules ()
addBuiltinRule = undefined

-- | Function representing a built-in rule. This function will be pre-applied to the
--   first two arguments, then called repeatedly as necessary in response to 'apply'
--   or values stored in the database.
type BuiltinRule key value
    =  ShakeOptions
        -- ^ The currently active 'ShakeOptions'.
    -> (forall u . Typeable u => Proxy u -> Maybe u)
        -- ^ A way to query 'UserRule' values at a particular type.
    -> key
        -- ^ Key that you want to build. Will be called at most once per key per run.
    -> Maybe BS.ByteString
        -- ^ 'Just' the previous result in the database, or 'Nothing' to indicate Shake has no memory of this rule.
        --   In most cases this will be a serialised value of type @value@.
    -> Action Bool
        -- ^ An 'Action' that if executed will return whether any dependencies from the previous execution have changed.
        --   Returns 'True' if any dependency has changed, or if Shake has no memory of this rule.
        --   Does not add any dependencies.
    -> Action (BuiltinInfo value)

data BuiltinInfo value = BuiltinInfo
    {changedDependencies :: Bool
        -- ^ Have the required dependencies of this action changed? Use 'True' to use the dependencies this time
        --   around as the future dependencies. Use 'False' to keep the previous dependencies.
    ,changedStore :: Bool
    ,resultStore :: BS.ByteString
        -- ^ Return the new value to store, and a 'True' if that value has changed from the argument store.
    ,changedValue :: Bool
    ,resultValue :: value
        -- ^ Return the produced value and a 'True' if that value has changed in a meaningful way from last time.
    }

---------------------------------------------------------------------
-- IMPLEMENTATION NOTES

{- ShakeValue

For @key@ I can stop using ShakeValue and switch to an explicit dictionary copying over just the 4 methods I care about.
Less runtime overhead and faster access, plus more explicit so easier to manipulate.

For @value@ all the operations have moved into the addBuiltinRule so I only need Typeable.
-}


{- DataBase

Switch to reading pieces a chunk at a time into a single strict block storage. Have 1 byte being the key/value entry,
the size of the key, then the size of the value. Don't deserialise the value at all, and many keys will be literal
values.
-}


{- ThreadPool

One of the effects of this change is that suddenly every rule will be checked in a separate ThreadPool entry.
That means the ThreadPool needs optimising further.
-}


{- Skip/Rebuild etc

The hope is that most rules won't care, but that those which do will implement it themselves from ShakeOptions.
-}


{- File Rule Composability

File rules are currently not very composable, the individual pieces cannot be recombined. Hopefully that's now possible,
but it's not clear what it would look like.
-}


{- Lint checking

Not sure what this looks like. Maybe addBuiltinRule takes a lint checker argument?
-}


{- Target printing

Just call addHelpTarget.
-}
