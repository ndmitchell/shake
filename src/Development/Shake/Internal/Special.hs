
-- | This module contains rule types that have special behaviour in some way.
--   Everything in this module is a hack.
module Development.Shake.Internal.Special(
    specialIsFileKey
    ) where

import Data.Typeable


specialIsFileKey :: TypeRep -> Bool
specialIsFileKey t = con == "FileQ"
    where con = show $ fst $ splitTyConApp t
