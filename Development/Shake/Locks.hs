
module Development.Shake.Locks(
    Lock, newLock, withLock,
    Var, newVar, readVar, modifyVar, modifyVar_,
    Barrier, newBarrier, signalBarrier, waitBarrier,
    Resource, newResource, acquireResource, releaseResource
    ) where

import Control.Concurrent
import Control.Monad


---------------------------------------------------------------------
-- LOCK

-- | Like an MVar, but has no value
newtype Lock = Lock (MVar ())
instance Show Lock where show _ = "Lock"

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)
instance Show (Var a) where show _ = "Var"

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once
newtype Barrier a = Barrier (MVar a)
instance Show (Barrier a) where show _ = "Barrier"

newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = putMVar x

waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x


---------------------------------------------------------------------
-- RESOURCE

-- | The type representing a finite resource, which multiple build actions should respect.
--   Created with 'newResource' in the 'IO' monad before calling 'Development.Shake.shake',
--   and used with 'Development.Shake.withResource' in the 'Development.Shake.Action' monad
--   when defining rules.
--
--   As an example, only one set of calls to the Excel API can occur at one time, therefore
--   Excel is a finite resource of quantity 1. You can write:
--
-- @
-- do excel <- 'newResource' \"Excel\" 1
--    'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=2} $ do
--        'Development.Shake.want' [\"a.xls\",\"b.xls\"]
--        \"*.xls\" 'Development.Shake.*>' \\out ->
--            'Development.Shake.withResource' excel 1 $
--                'Development.Shake.system'' \"excel\" [out,...]
--  @
--
--   Now the two calls to @excel@ will not happen in parallel. Using 'Resource'
--   is better than 'MVar' as it will not block any other threads from executing.
--
--   As another example, calls to compilers are usually CPU bound but calls to linkers are usually
--   disk bound. Running 8 linkers will often cause an 8 CPU system to grid to a halt. We can limit
--   ourselves to 4 linkers with:
--
-- @
-- do disk <- 'newResource' \"Disk\" 4
--    'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=8} $ do
--        'Development.Shake.want' [show i 'Development.Shake.FilePath.<.>' \"exe\" | i <- [1..100]]
--        \"*.exe\" 'Development.Shake.*>' \\out ->
--            'Development.Shake.withResource' disk 1 $
--                'Development.Shake.system'' \"ld\" [\"-o\",out,...]
--        \"*.o\" 'Development.Shake.*>' \\out ->
--            'Development.Shake.system'' \"cl\" [\"-o\",out,...]
-- @
data Resource = Resource String Int (Var (Int,[(Int,IO ())]))
instance Show Resource where show (Resource name _ _) = "Resource " ++ name


-- | Create a new finite resource, given a name (for error messages) and a quantity of the resource that exists.
--   For an example see 'Resource'.
newResource :: String -> Int -> IO Resource
newResource name mx = do
    when (mx < 0) $
        error $ "You cannot create a resource named " ++ name ++ " with a negative quantity, you used " ++ show mx
    var <- newVar (mx, [])
    return $ Resource name mx var


-- | Try to acquire a resource. Returns Nothing to indicate you have acquired with no blocking, or Just act to
--   say after act completes (which will block) then you will have the resource.
acquireResource :: Resource -> Int -> IO (Maybe (IO ()))
acquireResource r@(Resource name mx var) want
    | want < 0 = error $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show want
    | want > mx = error $ "You cannot acquire more than " ++ show mx ++ " of " ++ show r ++ ", requested " ++ show want
    | otherwise = modifyVar var $ \(available,waiting) ->
        if want <= available then
            return ((available - want, waiting), Nothing)
        else do
            bar <- newBarrier
            return ((available, waiting ++ [(want,signalBarrier bar ())]), Just $ waitBarrier bar)


-- | You should only ever releaseResource that you obtained with acquireResource.
releaseResource :: Resource -> Int -> IO ()
releaseResource (Resource name mx var) i = modifyVar_ var $ \(available,waiting) -> f (available+i) waiting
    where
        f i ((wi,wa):ws) | wi <= i = wa >> f (i-wi) ws
                         | otherwise = do (i,ws) <- f i ws; return (i,(wi,wa):ws)
        f i [] = return (i, [])
