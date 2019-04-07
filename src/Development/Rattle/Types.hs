{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Development.Rattle.Types(
    Trace(..), fsaTrace,
    Cmd(..),
    T, t0,
    ) where

import Data.Hashable
import Data.List.Extra
import Development.Shake.Command


newtype Cmd = Cmd [String]
    deriving (Show, Read, Eq, Hashable)

data Trace a = Trace
    {tRead :: [(FilePath, a)]
    ,tWrite :: [(FilePath, a)]
    } deriving (Show, Read, Functor, Foldable, Traversable)

instance Semigroup (Trace a) where
    Trace r1 w1 <> Trace r2 w2 = Trace (r1++r2) (w1++w2)

instance Monoid (Trace a) where
    mempty = Trace [] []


fsaTrace :: [FSATrace] -> Trace ()
fsaTrace = nubTrace . mconcat . map f
    where
        g r w = Trace (map (,()) r) (map (,()) w)
        f (FSAWrite x) = g [] [x]
        f (FSARead x) = g [x] []
        f (FSADelete x) = g [] [x]
        f (FSAMove x y) = g [] [x,y]
        f (FSAQuery x) = g [x] []
        f (FSATouch x) = g [] [x]

nubTrace :: Ord a => Trace a -> Trace a
nubTrace (Trace a b) = Trace (nubOrd a \\ b) (nubOrd b)


newtype T = T Int -- timestamps
    deriving (Enum,Eq,Ord,Show)

t0 :: T
t0 = T 0
