
-- | The endpoints on the cloud server
module Development.Shake.Internal.History.Server(
    Server, BuildTree(..), noBuildTree,
    newServer,
    serverAllKeys, serverOneKey, serverDownloadFiles,
    serverUpload
    ) where

import Development.Shake.Internal.History.Bloom
import Development.Shake.Internal.Value
import General.Binary
import General.Extra
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Network
import Data.Typeable


data BuildTree
    = Depend [Key] [([BS_Identity], BuildTree)]
    | Done BS_Store [(FileHash, FilePath)]

noBuildTree :: BuildTree
noBuildTree = Depend [] []

data Server = Server Conn (BinaryOp Key) Ver

newServer :: Conn -> BinaryOp Key -> Ver -> IO Server
newServer a b c = return $ Server a b c

serverAllKeys :: Server -> [(TypeRep, Ver)] -> IO [(Key, Ver, [Int], Bloom [BS_Identity])]
serverAllKeys _ _ = return []

serverOneKey :: Server -> Key -> Ver -> Ver -> [(Key, BS_Identity)] -> IO BuildTree
serverOneKey _ _ _ _ _ = return $ Depend [] []


serverDownloadFiles :: Server -> Key -> [(FileHash, FilePath)] -> IO ()
serverDownloadFiles _ _ _ = fail "Failed to download the files"


serverUpload :: Server -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
serverUpload _ key _ _ _ _ _  = print ("SERVER", "Uploading key", key)
