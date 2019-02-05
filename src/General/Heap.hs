-- Copied from https://github.com/ekmett/heaps/blob/87c2c8098270d6cf76f1017182c1f7effa8357e6/src/Data/Heap.hs
-- Because I can't depend on 'heaps', see https://github.com/ekmett/heaps/issues/13
-- I removed the CPP

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2010-2015
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient, asymptotically optimal, implementation of a priority queues
-- extended with support for efficient size, and `Data.Foldable`
--
-- /Note/: Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Heap (Heap)
-- >  import qualified Data.Heap as Heap
--
-- The implementation of 'Heap' is based on /bootstrapped skew binomial heaps/
-- as described by:
--
--    * G. Brodal and C. Okasaki , <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.48.973 "Optimal Purely Functional Priority Queues">,
--      /Journal of Functional Programming/ 6:839-857 (1996)
--
-- All time bounds are worst-case.
-----------------------------------------------------------------------------

module General.Heap
    (
    -- * Heap Type
      Heap -- instance Eq,Ord,Show,Read,Data,Typeable
    -- * Entry type
    , Entry(..) -- instance Eq,Ord,Show,Read,Data,Typeable
    -- * Basic functions
    , empty             -- O(1) :: Heap a
    , insert            -- O(1) :: Ord a => a -> Heap a -> Heap a
    , uncons            -- O(1)\/O(log n) :: Heap a -> Maybe (a, Heap a)
    ) where

import Data.Monoid
import Data.Foldable hiding (minimum, concatMap)
import Data.Function (on)
import Prelude hiding (foldr)

-- The implementation of 'Heap' must internally hold onto the dictionary entry for ('<='),
-- so that it can be made 'Foldable'. Confluence in the absence of incoherent instances
-- is provided by the fact that we only ever build these from instances of 'Ord' a (except in the case of 'groupBy')


-- | A min-heap of values of type @a@.
data Heap a
  = Empty
  | Heap {-# UNPACK #-} !Int (a -> a -> Bool) {-# UNPACK #-} !(Tree a)

instance Show a => Show (Heap a) where
  showsPrec _ Empty = showString "fromList []"
  showsPrec d (Heap _ _ t) = showParen (d > 10) $
    showString "fromList " .  showsPrec 11 (toList t)

-- | /O(1)/. The empty heap
--
-- @'empty' ≡ 'fromList' []@
--
-- >>> size empty
-- 0
empty :: Heap a
empty = Empty
{-# INLINE empty #-}


singletonWith :: (a -> a -> Bool) -> a -> Heap a
singletonWith f a = Heap 1 f (Node 0 a Nil)
{-# INLINE singletonWith #-}

-- | /O(1)/. Insert a new value into the heap.
--
-- >>> insert 2 (fromList [1,3])
-- fromList [1,2,3]
--
-- @
-- 'insert' x 'empty' ≡ 'singleton' x
-- 'size' ('insert' x xs) ≡ 1 + 'size' xs
-- @
insert :: Ord a => a -> Heap a -> Heap a
insert = insertWith (<=)
{-# INLINE insert #-}

insertWith :: (a -> a -> Bool) -> a -> Heap a -> Heap a
insertWith leq x Empty = singletonWith leq x
insertWith leq x (Heap s _ t@(Node _ y f))
  | leq x y   = Heap (s+1) leq (Node 0 x (t `Cons` Nil))
  | otherwise = Heap (s+1) leq (Node 0 y (skewInsert leq (Node 0 x Nil) f))
{-# INLINE insertWith #-}


-- | Provides both /O(1)/ access to the minimum element and /O(log n)/ access to the remainder of the heap.
-- This is the same operation as 'viewMin'
--
-- >>> uncons (fromList [2,1,3])
-- Just (1,fromList [2,3])
uncons :: Heap a -> Maybe (a, Heap a)
uncons Empty = Nothing
uncons l@(Heap _ _ t) = Just (root t, deleteMin l)
{-# INLINE uncons #-}


trees :: Forest a -> [Tree a]
trees (a `Cons` as) = a : trees as
trees Nil = []

-- | /O(log n)/. Delete the minimum key from the heap and return the resulting heap.
--
-- >>> deleteMin (fromList [3,1,2])
-- fromList [2,3]
deleteMin :: Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Heap _ _ (Node _ _ Nil)) = Empty
deleteMin (Heap s leq (Node _ _ f0)) = Heap (s - 1) leq (Node 0 x f3)
  where
    (Node r x cf, ts2) = getMin leq f0
    (zs, ts1, f1) = splitForest r Nil Nil cf
    f2 = skewMeld leq (skewMeld leq ts1 ts2) f1
    f3 = foldr (skewInsert leq) f2 (trees zs)
{-# INLINE deleteMin #-}

-- * Subranges

-- we hold onto the children counts in the nodes for /O(1)/ 'size'
data Tree a = Node
  { rank :: {-# UNPACK #-} !Int
  , root :: a
  , _forest :: !(Forest a)
  } deriving (Show)

data Forest a = !(Tree a) `Cons` !(Forest a) | Nil
  deriving (Show)
infixr 5 `Cons`

instance Functor Tree where
  fmap f (Node r a as) = Node r (f a) (fmap f as)

instance Functor Forest where
  fmap f (a `Cons` as) = fmap f a `Cons` fmap f as
  fmap _ Nil = Nil

-- internal foldable instances that should only be used over commutative monoids
instance Foldable Tree where
  foldMap f (Node _ a as) = f a `mappend` foldMap f as

-- internal foldable instances that should only be used over commutative monoids
instance Foldable Forest where
  foldMap f (a `Cons` as) = foldMap f a `mappend` foldMap f as
  foldMap _ Nil = mempty

link :: (a -> a -> Bool) -> Tree a -> Tree a -> Tree a
link f t1@(Node r1 x1 cf1) t2@(Node r2 x2 cf2) -- assumes r1 == r2
  | f x1 x2   = Node (r1+1) x1 (t2 `Cons` cf1)
  | otherwise = Node (r2+1) x2 (t1 `Cons` cf2)

skewLink :: (a -> a -> Bool) -> Tree a -> Tree a -> Tree a -> Tree a
skewLink f t0@(Node _ x0 cf0) t1@(Node r1 x1 cf1) t2@(Node r2 x2 cf2)
  | f x1 x0 && f x1 x2 = Node (r1+1) x1 (t0 `Cons` t2 `Cons` cf1)
  | f x2 x0 && f x2 x1 = Node (r2+1) x2 (t0 `Cons` t1 `Cons` cf2)
  | otherwise          = Node (r1+1) x0 (t1 `Cons` t2 `Cons` cf0)

ins :: (a -> a -> Bool) -> Tree a -> Forest a -> Forest a
ins _ t Nil = t `Cons` Nil
ins f t (t' `Cons` ts) -- assumes rank t <= rank t'
  | rank t < rank t' = t `Cons` t' `Cons` ts
  | otherwise = ins f (link f t t') ts

uniqify :: (a -> a -> Bool) -> Forest a -> Forest a
uniqify _ Nil = Nil
uniqify f (t `Cons` ts) = ins f t ts

unionUniq :: (a -> a -> Bool) -> Forest a -> Forest a -> Forest a
unionUniq _ Nil ts = ts
unionUniq _ ts Nil = ts
unionUniq f tts1@(t1 `Cons` ts1) tts2@(t2 `Cons` ts2) = case compare (rank t1) (rank t2) of
  LT -> t1 `Cons` unionUniq f ts1 tts2
  EQ -> ins f (link f t1 t2) (unionUniq f ts1 ts2)
  GT -> t2 `Cons` unionUniq f tts1 ts2

skewInsert :: (a -> a -> Bool) -> Tree a -> Forest a -> Forest a
skewInsert f t ts@(t1 `Cons` t2 `Cons`rest)
  | rank t1 == rank t2 = skewLink f t t1 t2 `Cons` rest
  | otherwise = t `Cons` ts
skewInsert _ t ts = t `Cons` ts
{-# INLINE skewInsert #-}

skewMeld :: (a -> a -> Bool) -> Forest a -> Forest a -> Forest a
skewMeld f ts ts' = unionUniq f (uniqify f ts) (uniqify f ts')
{-# INLINE skewMeld #-}

getMin :: (a -> a -> Bool) -> Forest a -> (Tree a, Forest a)
getMin _ (t `Cons` Nil) = (t, Nil)
getMin f (t `Cons` ts)
  | f (root t) (root t') = (t, ts)
  | otherwise            = (t', t `Cons` ts')
  where (t',ts') = getMin f ts
getMin _ Nil = error "Heap.getMin: empty forest"

splitForest :: Int -> Forest a -> Forest a -> Forest a -> (Forest a, Forest a, Forest a)
splitForest a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
splitForest 0 zs ts f = (zs, ts, f)
splitForest 1 zs ts (t `Cons` Nil) = (zs, t `Cons` ts, Nil)
splitForest 1 zs ts (t1 `Cons` t2 `Cons` f)
  -- rank t1 == 0
  | rank t2 == 0 = (t1 `Cons` zs, t2 `Cons` ts, f)
  | otherwise    = (zs, t1 `Cons` ts, t2 `Cons` f)
splitForest r zs ts (t1 `Cons` t2 `Cons` cf)
  -- r1 = r - 1 or r1 == 0
  | r1 == r2          = (zs, t1 `Cons` t2 `Cons` ts, cf)
  | r1 == 0           = splitForest (r-1) (t1 `Cons` zs) (t2 `Cons` ts) cf
  | otherwise         = splitForest (r-1) zs (t1 `Cons` ts) (t2 `Cons` cf)
  where
    r1 = rank t1
    r2 = rank t2
splitForest _ _ _ _ = error "Heap.splitForest: invalid arguments"


-- | Explicit priority/payload tuples. Useful to build a priority queue using
-- a 'Heap', since the payload is ignored in the Eq/Ord instances.
--
-- @
-- myHeap = 'fromList' ['Entry' 2 \"World", 'Entry' 1 \"Hello", 'Entry' 3 "!"]
--
-- ==> 'foldMap' 'payload' myHeap ≡ "HelloWorld!"
-- @
data Entry p a = Entry { priority :: p, payload :: a }
  deriving (Show)

-- instance Comonad (Entry p) where
--   extract (Entry _ a) = a
--   extend f pa@(Entry p _) Entry p (f pa)

instance Eq p => Eq (Entry p a) where
  (==) = (==) `on` priority
  {-# INLINE (==) #-}

instance Ord p => Ord (Entry p a) where
  compare = compare `on` priority
  {-# INLINE compare #-}