
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    Rule(..), EqualCost(..), rule, apply, apply1,
    trackUse, trackChange, trackAllow,
    ) where

import Development.Shake.Core
import Development.Shake.Types
