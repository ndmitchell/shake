
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
import Data.List.Extra
import qualified General.Ids as Ids
import qualified Data.HashMap.Strict as Map
import Data.Typeable
import General.Binary
import General.Extra
import General.Wait
import Data.Functor
import Prelude


data Initial = Initial (Map.HashMap Key Ids.Id) (Ids.Ids (Ver, [Ids.Id], [BS_Identity] -> Bool))

data Cloud = Cloud Server (Barrier Initial)

newCloud :: BinaryOp Key -> Ver -> [(TypeRep, Ver)] -> [String] -> Maybe (IO Cloud)
newCloud binop globalVer ruleVer urls = flip fmap (if null urls then Nothing else connect $ last urls) $ \conn -> do
    conn <- conn
    server <- newServer conn binop globalVer
    bar <- newBarrier
    forkFinally (timeout 20 $ serverAllKeys server ruleVer) $ \res -> signalBarrier bar =<< case res of
        Right (Just xs) -> Initial
            (Map.fromList [(k,Ids.Id $ fromIntegral i) | (i,(k,_,_,_)) <- zipFrom 0 xs]) <$>
            Ids.fromList [(a,map (Ids.Id . fromIntegral) b,c) | (_,a,b,c) <- xs]
        _ -> Initial Map.empty <$> Ids.empty
    return $ Cloud server bar


addCloud :: Cloud -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
addCloud (Cloud server _) x1 x2 x3 x4 x5 x6 = void $ forkIO $ serverUpload server x1 x2 x3 x4 x5 x6


lookupCloud :: Cloud -> (Locked () -> IO ()) -> (Key -> Locked (Wait (Maybe BS_Identity))) -> Key -> Ver -> Ver -> Locked (Wait (Maybe (BS_Store, [[Key]], IO ())))
lookupCloud (Cloud server initial) relock ask key builtinVer userVer = return $ Later $ \c ->
    void $ liftIO $ flip forkFinally (relock . c . either (const Nothing) join) $
        timeout 5 $ do
            Initial mp ids <- waitBarrier initial
            Just id <- return $ Map.lookup key mp
            Just (ver, deps, test) <- Ids.lookup ids id
            when (ver /= userVer) $ fail ""
            Just ks <- sequence <$> mapM (Ids.lookup ids) deps
            return Nothing
