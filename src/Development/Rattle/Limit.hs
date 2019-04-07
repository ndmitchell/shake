{-# LANGUAGE TupleSections #-}

module Development.Rattle.Limit(
    Limit, newLimit, withLimit, withLimitMaybe,
    ) where

import qualified General.Bilist as B
import Control.Exception
import Control.Monad
import Control.Concurrent.Extra


newtype Limit = Limit (Var S)

data S = Free !Int
       | Queued (B.Bilist (IO ()))


newLimit :: Int -> IO Limit
newLimit i
    | i < 1 = error $ "newLimit, argument must be >= 1, got " ++ show i
    | otherwise = Limit <$> newVar (Free i)

data LS
    = Wait -- I am waiting like normal, default state
    | Fire -- I got asked to run
    | Died -- I got an async exception

withLimit :: Limit -> IO a -> IO a
withLimit (Limit var) act = mask $ \unmask ->
    join $ modifyVar var $ \x -> case x of
        Free i | i >= 1 -> return $ (Free (i-1),) $ do
            res <- unmask act `onException` finished var
            finished var
            return res
        _ -> do
            let q = case x of Queued q -> q; _ -> mempty
            -- a real pain to deal with async exceptions while waiting
            -- but I think this does it - although not very elegant
            wait <- newBarrier
            ls <- newVar Wait
            let go = join $ modifyVar ls $ \x -> return $ case x of
                    Wait -> (Fire, signalBarrier wait ())
                    Died -> (Died, finished var)
            return $ (Queued $ B.cons go q, ) $ do
                unmask (waitBarrier wait) `onException` do
                    join $ modifyVar ls $ \x -> return $ case x of
                        Wait -> (Died, return ())
                        Fire -> (Fire, finished var)
                res <- unmask act `onException` finished var
                finished var
                return res


withLimitMaybe :: Limit -> IO a -> IO (Maybe a)
withLimitMaybe (Limit var) act = mask $ \unmask ->
    join $ modifyVar var $ \x -> case x of
        Free i | i >= 1 -> return $ (Free (i-1),) $ do
            res <- unmask act `onException` finished var
            finished var
            return $ Just res
        _ -> return (x, return Nothing)


finished :: Var S -> IO ()
finished var = mask_ $
    join $ modifyVar var $ \x -> case x of
        Free i -> return (Free (i+1), return ())
        Queued q -> case B.uncons q of
            Nothing -> return (Free 1, return ())
            Just (q, qs) -> return (Queued qs, q)
