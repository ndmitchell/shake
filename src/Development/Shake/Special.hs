
-- | This module contains rule types that have special behaviour in some way.
--   Everything in this module is a hack.
module Development.Shake.Special(
    specialAlwaysRebuilds,
    specialIsFileKey
    ) where

import Development.Shake.Value
import Data.Typeable


specialAlwaysRebuilds :: Value -> Bool
specialAlwaysRebuilds v = con `elem` ["AlwaysRerunA","OracleA","DoesFileExistA","DoesDirectoryExistA","GetDirectoryA","GetEnvA"]
                            || (con == "FileA" && show v == "File {mod=NEQ,size=NEQ,digest=NEQ}")
    where con = show $ fst $ splitTyConApp $ typeValue v


specialIsFileKey :: TypeRep -> Bool
specialIsFileKey t = con == "FileQ"
    where con = show $ fst $ splitTyConApp t
