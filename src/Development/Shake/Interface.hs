{-# LANGUAGE Rank2Types #-}

-- | Proposed new interface for defining custom rules.
--
--   This interface is meant to be deliberately low-level. The idea is to allow anything that
--   makes sense to be implemented efficiently and correctly.
--
--   It is expected most people will use higher-level facilities such as 'addOracle' or other
--   appropriate sugar.
--
--   Q: What is the appropriate sugar? Something like @storedValueE@ from the PR?
--   Q: Are these really the right names?
module Development.Shake.Interface(
    Encoder(..),
    UserRule(..),
    addBuiltinRule, BuiltinRule
    ) where

import Development.Shake.Classes
import Development.Shake.Types
import Development.Shake.Core
import Data.Proxy
import qualified Data.ByteString as BS


---------------------------------------------------------------------
-- BINARY SERIALISATION

-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--
--   Q: Should this be ByteString or a primitive ByteArray?
class Encoder a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> a


---------------------------------------------------------------------
-- USER RULES

-- | A 'Match' data type, representing user-defined rules associated with a particular type.
--   As an example '?>' and '*>' will add entries to the 'Match' data type.
data UserRule a
    = UserRule a -- ^ Added to the state with @'addUserRule' :: Typeable a => a -> Rules ()@.
    | Priority Double (UserRule a) -- ^ Rules defined under 'priority'.
    | Unordered [UserRule a] -- ^ Rules defined normally, at most one should match.
    | Ordered [UserRule a] -- ^ Rules defined under 'alternative', matched in order.


---------------------------------------------------------------------
-- DEFINING RULES

-- | Add a new rule, which maps between keys and values. For every distinct key/value
--   pair there may be at most one rule defined.
addBuiltinRule
    :: (Encoder key, Hashable key, Eq key, Typeable key, Typeable value)
    => BuiltinRule key value -> Rules ()
addBuiltinRule = undefined

-- | Function representing a built-in rule. This function will be pre-applied to the
--   first two arguments, then called repeatedly as necessary in response to 'apply'.
type BuiltinRule key value
    =  ShakeOptions
        -- ^ The currently active 'ShakeOptions'.
    -> (forall a . Typeable a => Proxy a -> UserRule a)
        -- ^ A way to query 'UserRule' values at a particular type.
    -> key
        -- ^ Key that you want to build.
    -> Maybe BS.ByteString
        -- ^ 'Just' the previous result in the database, or 'Nothing' to indicate Shake has no memory of this rule.
        --   In most cases this will be a serialised value of type @value@.
    -> Action Bool
        -- ^ An 'Action' that if executed will return whether any dependencies from the previous execution have changed.
        --   Returns 'True' if any dependency has changed, or if Shake has no memory of this rule.
        --   Does not add any dependencies.
    -> Action
        (Bool
            -- ^ Have the required dependencies of this action changed? Use 'True' to use the dependencies this time
            --   around as the future dependencies. Use 'False' to keep the previous dependencies.
        ,BS.ByteString, Bool
            -- ^ Return the new value to store, and a 'True' if that value has changed from the argument store.
        ,value, Bool
            -- ^ Return the produced value and a 'True' if that value has changed in a meaningful way from last time.
        )


---------------------------------------------------------------------
-- BUILTINRULE SKETCHES

{- File

The key will be a ByteArray, optimally shaped to pass to the modtime functions.
On Linux, always in UTF8.
On Windows, either in ASCII, or if the first byte is a 0, in UTF8.
The key will not be in UTF8 unless it cannot be represented in ASCII, so we have canonical values.

We can do more efficient UserRule precomputations so that all `%>` patterns on literals go into a HashMap.

The complicated dance about having the hash not computed until it's needed can be dropped, so no relying on
unsafePerformIO.
-}


{- Phony

The key will be a file-compatible ByteArray. The result will be () and take 0 bytes.
It can now overlap with the File rule without having special logic.
-}


{- GetDirectoryFiles

The main change is that instead of storing the whole value in the value ByteString we can just store a hash.
Since we don't have to produce value from the ByteString, and have to do the directory scan anyway, hopefully
this will significantly reduce the size of the database. Might also be applicable in lots of other cases.
-}


{- Oracles

They can stay the same, still encoded using the Binary interface. Adding an option to never rerun them is easy too.
-}


{- AlwaysRerun

Nothing complicated, just no need to fake up an Eq that doesn't match in all the right ways.
-}


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

Printing out all available targets should be possible based on the Match data type, just figuring out
the right UI to let the rule do so. Perhaps an extra argument?
-}
