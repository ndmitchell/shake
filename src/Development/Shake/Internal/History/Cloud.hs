
-- | The endpoints on the server
module Development.Shake.Internal.History.Cloud(
    Cloud, newCloud, addCloud, lookupCloud
    ) where

import Development.Shake.Internal.Value
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Network
import Development.Shake.Internal.History.Server
import Control.Concurrent.Extra
import System.Time.Extra
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import General.Fence
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import Data.Typeable
import General.Binary
import General.Extra
import General.Wait
import Data.Maybe
import Prelude


type Initial = Map.HashMap Key (Ver, [Key], [BS_Identity] -> Bool)

data Cloud = Cloud Server (Locked () -> IO ()) (Fence Locked Initial)


newLaterFence :: (Locked () -> IO ()) -> Seconds -> a -> IO a -> IO (Fence Locked a)
newLaterFence relock maxTime def act = do
    fence <- newFence
    forkFinally (timeout maxTime act) $ \res -> relock $ signalFence fence $ case res of
        Right (Just v) -> v
        _ -> def
    return fence

laterFence :: MonadIO m => Fence m a -> Wait m a
laterFence fence = do
    res <- liftIO $ testFence fence
    case res of
        Just v -> return v
        Nothing -> Later $ waitFence fence


newCloud :: (Locked () -> IO ()) -> BinaryOp Key -> Ver -> [(TypeRep, Ver)] -> [String] -> Maybe (IO Cloud)
newCloud relock binop globalVer ruleVer urls = flip fmap (if null urls then Nothing else connect $ last urls) $ \conn -> do
    conn <- conn
    server <- newServer conn binop globalVer
    fence <- newLaterFence relock 10 Map.empty $ do
        xs <- serverAllKeys server ruleVer
        let at = fastAt [k | (k,_,_,_) <- xs]
        return $ Map.fromList [(k,(v,mapMaybe at ds,test)) | (k,v,ds,test) <- xs]
    return $ Cloud server relock fence


addCloud :: Cloud -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
addCloud (Cloud server _ _) x1 x2 x3 x4 x5 x6 = void $ forkIO $ serverUpload server x1 x2 x3 x4 x5 x6


lookupCloud :: Cloud -> (Key -> Wait Locked (Maybe BS_Identity)) -> Key -> Ver -> Ver -> Wait Locked (Maybe (BS_Store, [[Key]], IO ()))
lookupCloud (Cloud server relock initial) ask key builtinVer userVer = runMaybeT $ do
    mp <- lift $ laterFence initial
    Just (ver, deps, test) <- return $ Map.lookup key mp
    when (ver /= userVer) $ fail ""
    fail ""
