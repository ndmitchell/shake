
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    -- * Defining builtin rules
    BuiltinLint, noLint, BuiltinRun, RunChanged(..), RunResult(..), addBuiltinRule,
    -- * Calling builtin rules
    apply, apply1,
    -- * User rules
    UserRule(..), addUserRule, getUserRules, userRuleMatch,
    -- * Lint integration
    trackUse, trackChange, trackAllow
    ) where

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
