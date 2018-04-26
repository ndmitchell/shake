
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    -- * Defining builtin rules
    addBuiltinRule,
    BuiltinLint, noLint, BuiltinIdentity, noIdentity, BuiltinRun, RunMode(..), RunChanged(..), RunResult(..),
    -- * Calling builtin rules
    apply, apply1,
    -- * User rules
    addUserRule, getUserRuleList, getUserRuleMaybe, getUserRuleOne,
    -- * Lint integration
    lintTrackRead, lintTrackWrite, lintTrackAllow,
    -- * Caching
    cacheAllow
    ) where

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.Core.Rules
