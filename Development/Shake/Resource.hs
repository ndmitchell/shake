{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Resource(
    Resource, newResourceIO, newThrottleIO, acquireResource, releaseResource
    ) where

import Development.Shake.Errors
import Development.Shake.Prelude
import Data.Function
import System.IO.Unsafe
import Control.Arrow
import Control.Monad


{-# NOINLINE resourceIds #-}
resourceIds :: Var Int
resourceIds = unsafePerformIO $ newVar 0

resourceId :: IO Int
resourceId = modifyVar resourceIds $ \i -> let j = i + 1 in j `seq` return (j, j)


-- | A type representing an external resource which the build system should respect. There
--   are two ways to create 'Resource's in Shake:
--
-- * 'Development.Shake.newResource' creates a finite resource, stopping too many actions running
--   simultaneously.
--
-- * 'Development.Shake.newThrottle' creates a throttled resource, stopping too many actions running
--   over a short time period.
--
--   These resources are used with 'Development.Shake.withResource' when defining rules. Typically only
--   system commands (such as 'Development.Shake.cmd') should be run inside 'Development.Shake.withResource',
--   not commands such as 'Development.Shake.need'.
--
--   Be careful that the actions run within 'Development.Shake.withResource' do not themselves require further
--   resources, or you may get a \"thread blocked indefinitely in an MVar operation\" exception.
--   If an action requires multiple resources, use 'Development.Shake.withResources' to avoid deadlock.
data Resource = Resource
    {resourceOrd :: Int
        -- ^ Key used for Eq/Ord operations. To make withResources work, we require newResourceIO < newThrottleIO
    ,resourceShow :: String
        -- ^ String used for Show
    ,acquireResource :: Int -> IO (Maybe (IO ()))
        -- ^ Try to acquire a resource. Returns Nothing to indicate you have acquired with no blocking, or Just act to
        --   say after act completes (which will block) then you will have the resource.
    ,releaseResource :: Int -> IO ()
        -- ^ You should only ever releaseResource that you obtained with acquireResource.
    }

instance Show Resource where show = resourceShow
instance Eq Resource where (==) = (==) `on` resourceOrd
instance Ord Resource where compare = compare `on` resourceOrd


---------------------------------------------------------------------
-- FINITE RESOURCES

-- | (number available, queue of people with how much they want and a barrier to signal when it is allocated to them)
type Finite = Var (Int, [(Int,Barrier ())])

-- | A version of 'Development.Shake.newResource' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newResource' instead.
newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = do
    when (mx < 0) $
        error $ "You cannot create a resource named " ++ name ++ " with a negative quantity, you used " ++ show mx
    key <- resourceId
    var <- newVar (mx, [])
    return $ Resource (negate key) shw (acquire var) (release var)
    where
        shw = "Resource " ++ name

        acquire :: Finite -> Int -> IO (Maybe (IO ()))
        acquire var want
            | want < 0 = error $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
            | want > mx = error $ "You cannot acquire more than " ++ show mx ++ " of " ++ shw ++ ", requested " ++ show want
            | otherwise = modifyVar var $ \(available,waiting) ->
                if want <= available then
                    return ((available - want, waiting), Nothing)
                else do
                    bar <- newBarrier
                    return ((available, waiting ++ [(want,bar)]), Just $ waitBarrier bar)

        release :: Finite -> Int -> IO ()
        release var i = modifyVar_ var $ \(available,waiting) -> f (available+i) waiting
            where
                f i ((wi,wa):ws) | wi <= i = signalBarrier wa () >> f (i-wi) ws
                                 | otherwise = do (i,ws) <- f i ws; return (i,(wi,wa):ws)
                f i [] = return (i, [])


---------------------------------------------------------------------
-- THROTTLE RESOURCES

data Throttle = Throttle
    {throttleLock :: Lock
        -- people queue up to grab from replenish, full means no one is queued
    ,throttleVal :: Var (Either (Barrier ()) [(Time, Int)])
        -- either someone waiting for resources, or the time to wait until before N resources become available
        -- anyone who puts a Barrier in the Left must be holding the Lock
    ,throttleTime :: IO Time
    }

-- | A version of 'Development.Shake.newThrottle' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newResource' instead.
newThrottleIO :: String -> Int -> Double -> IO Resource
newThrottleIO name count period = do
    when (count < 0) $
        error $ "You cannot create a throttle named " ++ name ++ " with a negative quantity, you used " ++ show count
    key <- resourceId
    lock <- newLock
    time <- offsetTime
    rep <- newVar $ Right [(0, count)]
    let s = Throttle lock rep time
    return $ Resource key shw (acquire s) (release s)
    where
        shw = "Throttle " ++ name

        release :: Throttle -> Int -> IO ()
        release Throttle{..} n = do
            t <- throttleTime
            modifyVar_ throttleVal $ \v -> case v of
                Left b -> signalBarrier b () >> return (Right [(t+period, n)])
                Right ts -> return $ Right $ ts ++ [(t+period, n)]

        acquire :: Throttle -> Int -> IO (Maybe (IO ()))
        acquire Throttle{..} want
            | want < 0 = error $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
            | want > count = error $ "You cannot acquire more than " ++ show count ++ " of " ++ shw ++ ", requested " ++ show want
            | otherwise = do
                let grab t vs = do
                        let (a,b) = span ((<= t) . fst) vs
                        -- renormalise for clock skew, nothing can ever be > t+period away
                        return (sum $ map snd a, map (first $ min $ t+period) b)
                let push i vs = [(0,i) | i > 0] ++ vs

                -- attempt to grab without locking
                res <- withLockTry throttleLock $ do
                    modifyVar throttleVal $ \v -> case v of
                        Right vs -> do
                            t <- throttleTime
                            (got,vs) <- grab t vs
                            if got >= want then
                                return (Right $ push (got - want) vs, True)
                             else
                                return (Right $ push got vs, False)
                        _ -> return (v, False)
                if res == Just True then
                    return Nothing
                 else
                    return $ Just $ withLock throttleLock $ do
                        -- keep trying to acquire more resources until you have everything you need
                        let f want = join $ modifyVar throttleVal $ \v -> case v of
                                Left _ -> err "newThrottle, invariant failed, Left while holding throttleLock"
                                Right vs -> do
                                    t <- throttleTime
                                    (got,vs) <- grab t vs
                                    case vs of
                                        _ | got >= want -> return (Right $ push (got - want) vs, return ())
                                        [] -> do
                                            b <- newBarrier
                                            return (Left b, waitBarrier b >> f (want - got))
                                        (t2,n):vs -> do
                                            -- be robust to clock skew - only ever sleep for 'period' at most and always mark the next as good.
                                            return $ (,) (Right $ (0,n):vs) $ do
                                                sleep $ min period (t2-t)
                                                f $ want - got
                        f want
