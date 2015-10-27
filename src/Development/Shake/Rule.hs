
-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
    ShakeValue,
    Rule(..), EqualCost(..), rule, apply, apply1,
    trackUse, trackChange, trackAllow,
    -- * Deprecated
    defaultRule
    ) where

import Development.Shake.Core
import Development.Shake.Types

{-# DEPRECATED defaultRule "Use 'rule' with 'priority' 0" #-}

-- | A deprecated way of defining a low priority rule. Defined as:
--
-- @
-- defaultRule = 'priority' 0 . 'rule'
-- @
defaultRule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
defaultRule = priority 0 . rule
