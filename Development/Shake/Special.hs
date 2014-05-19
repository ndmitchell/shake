
-- | This module contains rule types that have special behaviour in some way.
--   Everything in this module is a hack.
module Development.Shake.Special(
    specialAlwaysRebuilds,
    specialIsFileKey
    ) where

import Development.Shake.Value
import Data.Typeable


specialAlwaysRebuilds :: Value -> Bool
specialAlwaysRebuilds v = con `elem` ["AlwaysRerunA","OracleA"] || (con == "FileA" && show v == "File {mod=0x7FFFFFFF,size=0,digest=0x0}")
    where con = show $ fst $ splitTyConApp $ typeValue v


specialIsFileKey :: TypeRep -> Bool
specialIsFileKey t = con == "FileQ"
    where con = show $ fst $ splitTyConApp t
