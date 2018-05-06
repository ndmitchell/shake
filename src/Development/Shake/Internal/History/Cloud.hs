
-- | The endpoints on the server
module Development.Shake.Internal.History.Cloud(
    Cloud, newCloud, addCloud, lookupCloud
    ) where

import Development.Shake.Internal.Value
import Development.Shake.Internal.History.Types
import Data.Typeable
import General.Binary
import General.Extra
import General.Wait


data Cloud = Cloud

newCloud :: BinaryOp Key -> Ver -> [(TypeRep, Ver)] -> [String] -> Maybe (IO Cloud)
newCloud _ _ _ _ = Cloud `seq` Nothing

addCloud :: Cloud -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
addCloud = undefined

lookupCloud :: Cloud -> (Key -> Locked (Wait (Maybe BS_Identity))) -> Key -> Ver -> Ver -> Locked (Wait (Maybe (BS_Store, [[Key]], IO ())))
lookupCloud = undefined
