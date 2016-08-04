
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    addUserRule, apply, apply1,
    addBuiltinRule, RunResult(..), RunChanged(..), BuiltinRun, BuiltinLint, noLint,
    trackUse, trackChange, trackAllow
    ) where

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
