
-- | The endpoints on the cloud server
module Development.Shake.Internal.History.Server(
    Server, BuildTree(..),
    newServer,
    serverAllKeys, serverOneKey, serverDownloadFiles,
    serverUpload
    ) where

import Development.Shake.Internal.Value
import General.Binary
import General.Extra
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Network
import Data.Typeable


data BuildTree
    = Depend [Key] [([BS_Identity], BuildTree)]
    | Done BS_Store [(FilePath, FileHash)]

data Server = Server Conn (BinaryOp Key) Ver

newServer :: Conn -> BinaryOp Key -> Ver -> IO Server
newServer a b c = return $ Server a b c

serverAllKeys :: Server -> [(TypeRep, Ver)] -> IO [(Key, Ver, [Int], [BS_Identity] -> Bool)]
serverAllKeys = undefined

serverOneKey :: Server -> Key -> Ver -> Ver -> [(Key, BS_Identity)] -> IO BuildTree
serverOneKey = undefined


serverDownloadFiles :: Server -> Key -> [(FileHash, FilePath)] -> IO ()
serverDownloadFiles = undefined


serverUpload :: Server -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
serverUpload = undefined
