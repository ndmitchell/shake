
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    Rule(..), EqualCost(..), rule, apply, apply1,
    trackUse, trackChange, trackAllow
    ) where

import Development.Shake.Internal.Core.Core
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Types
