
module General.Bilist(
    Bilist, cons, snoc, uncons, toList, isEmpty
    ) where

import Control.Arrow
import Data.Maybe
import Data.Monoid


newtype Bilist a = Bilist (Maybe (Tree a))

data Tree a = Leaf a | Branch (Tree a) (Tree a)

toList :: Bilist a -> [a]
toList (Bilist Nothing) = []
toList (Bilist (Just x)) = f x []
    where f (Leaf x) = (x:)
          f (Branch x y) = f x . f y

isEmpty :: Bilist a -> Bool
isEmpty (Bilist x) = isNothing x

instance Eq a => Eq (Bilist a) where
    a == b = toList a == toList b

instance Monoid (Bilist a) where
    mempty = Bilist Nothing
    mappend (Bilist Nothing) x = x
    mappend x (Bilist Nothing) = x
    mappend (Bilist (Just x)) (Bilist (Just y)) = Bilist $ Just $ Branch x y

one x = Bilist $ Just $ Leaf x

cons :: a -> Bilist a -> Bilist a
cons x xs = one x `mappend` xs

snoc :: Bilist a -> a -> Bilist a
snoc xs x = xs `mappend` one x

uncons :: Bilist a -> Maybe (a, Bilist a)
uncons (Bilist Nothing) = Nothing
uncons (Bilist (Just x)) = Just $ f x
    where f (Leaf x) = (x, mempty)
          f (Branch x y) = second (`mappend` Bilist (Just y)) $ f x
