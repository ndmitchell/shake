{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Monoid required < 7.9

module Development.Shake.Resource(
    Resource, newResourceIO, newThrottleIO, acquireResource, releaseResource
    ) where

import Data.Function
import Data.Monoid
import System.IO.Unsafe
import Control.Concurrent.Extra
import Data.Tuple.Extra
import Control.Monad
import General.Bilist
import Development.Shake.Pool
import System.Time.Extra


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
    ,acquireResource :: Pool -> Int -> IO () -> IO ()
        -- ^ Acquire the resource and call the function. Passes 'False' to indicate you have acquired with no blocking,
        --   or 'True' to say there was waiting and you must not do significant computation on that thread.
    ,releaseResource :: Pool -> Int -> IO ()
        -- ^ You should only ever releaseResource that you obtained with acquireResource.
    }

instance Show Resource where show = resourceShow
instance Eq Resource where (==) = (==) `on` resourceOrd
instance Ord Resource where compare = compare `on` resourceOrd


---------------------------------------------------------------------
-- FINITE RESOURCES

data Finite = Finite
    {finiteAvailable :: !Int
        -- ^ number of currently available resources
    ,finiteWaiting :: Bilist (Int, IO ())
        -- ^ queue of people with how much they want and the action when it is allocated to them
    }

-- | A version of 'Development.Shake.newResource' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newResource' instead.
newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = do
    when (mx < 0) $
        error $ "You cannot create a resource named " ++ name ++ " with a negative quantity, you used " ++ show mx
    key <- resourceId
    var <- newVar $ Finite mx mempty
    return $ Resource (negate key) shw (acquire var) (release var)
    where
        shw = "Resource " ++ name

        acquire :: Var Finite -> Pool -> Int -> IO () -> IO ()
        acquire var pool want continue
            | want < 0 = error $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
            | want > mx = error $ "You cannot acquire more than " ++ show mx ++ " of " ++ shw ++ ", requested " ++ show want
            | otherwise = join  $ modifyVar var $ \x@Finite{..} -> return $
                if want <= finiteAvailable then
                    (x{finiteAvailable = finiteAvailable - want}, continue)
                else
                    (x{finiteWaiting = finiteWaiting `snoc` (want, addPool pool continue)}, return ())

        release :: Var Finite -> Pool -> Int -> IO ()
        release var _ i = join $ modifyVar var $ \x -> return $ f x{finiteAvailable = finiteAvailable x + i}
            where
                f (Finite i (uncons -> Just ((wi,wa),ws)))
                    | wi <= i = second (wa >>) $ f $ Finite (i-wi) ws
                    | otherwise = first (add (wi,wa)) $ f $ Finite i ws
                f (Finite i _) = (Finite i mempty, return ())
                add a s = s{finiteWaiting = a `cons` finiteWaiting s}


---------------------------------------------------------------------
-- THROTTLE RESOURCES


-- call a function after a certain delay
waiter :: Seconds -> IO () -> IO ()
waiter period act = void $ forkIO $ do
    sleep period
    act

-- Make sure the pool cannot run try until after you have finished with it
blockPool :: Pool -> IO (IO ())
blockPool pool = do
    bar <- newBarrier
    addPool pool $ do
        cancel <- increasePool pool
        waitBarrier bar
        cancel
    return $ signalBarrier bar ()


data Throttle
      -- | Some number of resources are available
    = ThrottleAvailable !Int
      -- | Some users are blocked (non-empty), plus an action to call once we go back to Available
    | ThrottleWaiting (IO ()) (Bilist (Int, IO ()))


-- | A version of 'Development.Shake.newThrottle' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newThrottle' instead.
newThrottleIO :: String -> Int -> Double -> IO Resource
newThrottleIO name count period = do
    when (count < 0) $
        error $ "You cannot create a throttle named " ++ name ++ " with a negative quantity, you used " ++ show count
    key <- resourceId
    var <- newVar $ ThrottleAvailable count
    return $ Resource key shw (acquire var) (release var)
    where
        shw = "Throttle " ++ name

        acquire :: Var Throttle -> Pool -> Int -> IO () -> IO ()
        acquire var pool want continue
            | want < 0 = error $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
            | want > count = error $ "You cannot acquire more than " ++ show count ++ " of " ++ shw ++ ", requested " ++ show want
            | otherwise = join $ modifyVar var $ \x -> case x of
                ThrottleAvailable i
                    | i >= want -> return (ThrottleAvailable $ i - want, continue)
                    | otherwise -> do
                        stop <- blockPool pool
                        return (ThrottleWaiting stop $ (want - i, addPool pool continue) `cons` mempty, return ())
                ThrottleWaiting stop xs -> return (ThrottleWaiting stop $ xs `snoc` (want, addPool pool continue), return ())

        release :: Var Throttle -> Pool -> Int -> IO ()
        release var pool n = waiter period $ join $ modifyVar var $ \x -> return $ case x of
                ThrottleAvailable i -> (ThrottleAvailable $ i+n, return ())
                ThrottleWaiting stop xs -> f stop n xs
            where
                f stop i (uncons -> Just ((wi,wa),ws))
                    | i >= wi = second (wa >>) $ f stop (i-wi) ws
                    | otherwise = (ThrottleWaiting stop $ (wi-i,wa) `cons` ws, return ())
                f stop i _ = (ThrottleAvailable i, stop)
