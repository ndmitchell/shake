{-# LANGUAGE Rank2Types, DeriveFunctor, TupleSections #-}

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
--
--   Q: Should I just map the whole database into memory? 17Mb is not that much...
--   A: No, makes compression way harder
module Development.Shake.Experiment.Interface(
    Encoder(..),
    addHelpTarget, addUserRule,
    UserRule(..), userRuleMatch,
    BuiltinRule, BuiltinInfo(..), addBuiltinRule
    ) where

import Development.Shake.Classes
import Development.Shake.Types
import Development.Shake.Core
import Data.Proxy
import Data.Maybe
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

instance Encoder BS.ByteString where
    encode = id
    decode = id

instance Encoder () where
    encode () = BS.empty
    decode _ = ()

instance Encoder Bool where
    encode False = BS.empty
    encode True = BS.singleton 0
    decode x = not $ BS.null x


---------------------------------------------------------------------
-- USER RULES

addHelpTarget :: String -> Rules ()
addHelpTarget = undefined

addUserRule :: Typeable a => a -> Rules ()
addUserRule = undefined

-- | A 'Match' data type, representing user-defined rules associated with a particular type.
--   As an example '?>' and '*>' will add entries to the 'Match' data type.
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


-- | Rules first since they might be able to be optimised in some cases
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
    -> (forall u . Typeable u => Proxy u -> Maybe u)
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
-- BUILTINRULE SKETCHES

{- File'

A Str ==> () rule. Phony always has a payload of 0. File has payload of FileA.
Forward has a payload of FileA that is not checked (plus a trailing byte so not the same size as FileA).

data U = Match -> (Phony (Action ()) | File (Action ()) | Forward (Action FileA))

if bytes are Nothing then run the rule
else if bytes == 0 then rerun the Phony
     if bytes == FileA then check the file
     if bytes == Files then check if rerun, and if yes, then rerun
-}

{- Files'

Value is a list of FileA values.
-}

{- File

The key will be a ByteArray, optimally shaped to pass to the modtime functions.
On Linux, always in UTF8.
On Windows, either in ASCII, or if the first byte is a 0, in UTF8.
The key will not be in UTF8 unless it cannot be represented in ASCII, so we have canonical values.

We can do more efficient UserRule precomputations so that all `%>` patterns on literals go into a HashMap.

The complicated dance about having the hash not computed until it's needed can be dropped, so no relying on
unsafePerformIO.

The value will be (), allowing anyone to register ByteString -> () functions in the set
-}


{- Files

Need to be built as two rules - given [A,B] rule, need an A rule and a B rule, plus an [A,B] rule.
The [A] rule probably wants to store the key and result of A, but it doesn't need to compute it separately.
Needs to store the Key because the key must always be serialised.
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



{-
THOUGHTS

Given that many things may exist in the same universe, e.g. phony/also/file
how do they compete for keys?

maybe addRule works for both builtin and user rules?

maybe some type of adjacent merging algebra?

class Match p k where
    match :: [(p, v)] -> k -> [v]

addRule :: Match p k => p -> (k -> Action Bool -> (Bool, Bool, Bool, ByteString, v)) -> Rules ()


-- given a key, give a list of the rules that match, typically the singleton, with names for each rule
addRule :: (k -> Maybe (String, Action Bool -> (Bool, Bool, Bool, ByteString, v))) -> Rules ()


p k [(String, v)]

class Match p where
    match :: [p k 

need does an apply :: [SpecialString] -> [()], so they all live in the same namespace!
Works because the ByteString and the value do not have to coexist


We need something that can match only on the class, nothing else!
So we don't have to resolve matches in advance.

Need a distinction between the STATIC rule (run if nothing has changed)
And the DYNAMIC rule (run if something has changed)

Can't execute arbitrary user predicates to figure out the STATIC rule
Therefore must be determined by type alone? Could output a special tag after?

A |need| has access to the key and value types, and could potentially manufacture a check type.

Matches should definitely be bulk matched, with priority taking precedence elsewhere and alternatives also.
That works out nicely.

Should String -> () be inhabited by many instances?

They each need a different checker, so if:

* The need can take no previous history


builtin rules: k v c rules.

user rules: k v rules.


---------------------------------------------------------
-- DILEMA!

Should phony and file both have the same value type or not?

The "check" rule needs to be unambiguous for a given type to get fast lookup.
The "execute" rule needs to be matching, and wants to fail over between them.


'need' does matching, tries to figure out what matches, returns information for 'check'.
'check' does a check if 'need' requires rerunning.


ANSWER:

Provide k=Str v=() rules!


check must declare rerun.


addRunRule :: (k -> Maybe (RunRule c v)) -> Rules ()


addCheckRule :: (k -> CheckRule c v) -> Rules ()

RunRule must provide: c, v, did I change?

Importantly, don't want to do pattern matching...


Maybe smashing them all together isn't the end of the world? Could add special-string rules? which only had dependencies?

In fact, phony is absolutely fine...


alias rules? String -> Action a

file rules, simple, direct
phony rules, return a 0 length bytestring, easy to put in the same framework
also rules, keys must be stored separately....


multifile :: (FilePath -> ([FilePath], Maybe ))


when you need a key, can you redirect them to a different key?
YES, but I'll definitely want such a mapping to be cached.


override multifile. only cached in memory.
basically gives two entries into the table.

alias :: (k1 -> Maybe k2) -> 

first k is NEVER stored in the table?


when you need on a multifile, are you need'ing a subset?
Yes, you are.

Perhaps multifile doesn't need to store the results at all?


[a,b] &%> \...


need a does actual computation and stores result
need b does a need of a, then continues

a %> \...
b %> need a

How is that _not_ equivalent?

a_b %> \...
a %> need a_b
b %> need a_b

Firstly, if a %&> does not produce b, it's not an error.

ANSWER: If b gets deleted a_b doesn't rerun.

so a_b needs a copy of the answer so it can invalidate itself if a or b changes.

does a need a copy of the answer?

not if it can get the answer from a_b...

The multifile rule would have to take a list of files, and return a list of which were changed.

Rule type: [FilePath] -> [Bool]

need on one of these special things would have to say whether it had changed.

Still need to duplicate the key...


a %> do
    action
    provide b

Is that equivalent?

If you provide something you should track it - it is an output dependency.

need b %> do
    ...


a_b %> do
    action
    provide a
    provide b

NO, not a viable option

a %> do


need with a way to forward on? each node needs to be separate.

should all rules be multiples?

and permit missing pieces?




HiRule file 


need *.hi *> do
    apply $ HiRule *

don't really want to rescan each file for content hashes, but it is cleaner....

or maybe it can be a no-check need?

the need doesn't 

any stored value is correct by construction, so don't store anything (0 bytes)

if all your dependencies are identical (dep check = False), you have not changed.
if dep check has changed, call need on the multifile rule you are associated with,
which will produce a list of FileA values.

So still need to store it, just in the case where nothing changes, you can give up.
It's store but not check!

HiRule ==> [FileA]

File ==> (), but storing a FileA, so check for equality with the underlying FileA
but only in the case where the FileA changes.

-}
