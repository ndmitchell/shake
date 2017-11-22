
module General.ListBuilder(
    ListBuilder, runListBuilder, newListBuilder
    ) where

import Data.Semigroup (Semigroup (..))
import Data.Monoid hiding ((<>))
import Prelude()

data ListBuilder a
    = Zero
    | One a
    | Add (ListBuilder a) (ListBuilder a)


instance Semigroup (ListBuilder a) where
    Zero <> x = x
    x <> Zero = x
    x <> y = Add x y

instance Monoid (ListBuilder a) where
    mempty = Zero
    mappend = (<>)

newListBuilder :: a -> ListBuilder a
newListBuilder = One

runListBuilder :: ListBuilder a -> [a]
runListBuilder x = f x []
    where
        f Zero acc = []
        f (One x) acc = x : acc
        f (Add x y) acc = f x (f y acc)
