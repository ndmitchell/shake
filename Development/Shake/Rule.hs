{-# LANGUAGE CPP #-}

-- | This module is used for defining new types of rules for Shake build systems.
--   Most users will find the built-in set of rules sufficient.
module Development.Shake.Rule(
#if __GLASGOW_HASKELL__ >= 704
    ShakeValue,
#endif
    Rule(..), defaultRule, rule, apply, apply1,
    trackUse, trackChange, trackAllow
    ) where

import Development.Shake.Core

-- | Like 'rule', but lower priority, if no 'rule' exists then 'defaultRule' is checked.
--   All default rules must be disjoint.
defaultRule :: Rule key value => (key -> Maybe (Action value)) -> Rules ()
defaultRule = priority 0 . rule
