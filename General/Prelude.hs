
-- | Alternative Prelude which exports more things, so we can be
--   warning-compatible with GHC 7.10.
module General.Prelude(
    module Prelude,
    Monoid(..), Applicative(..)
    ) where

import Data.Monoid
import Control.Applicative
