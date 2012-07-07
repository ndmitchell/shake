{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Development.Shake.Database(
    Time, startTime, Duration, duration, Trace,
    Database, withDatabase,
    Ops(..), build,
    Stack, emptyStack, showStack,
    allEntries, showJSON, checkValid,
    ) where

import Development.Shake.Binary
import Development.Shake.Pool
import Development.Shake.Value
import Development.Shake.Locks
import Development.Shake.Storage

import Control.DeepSeq
import Data.Hashable
import Data.Typeable
import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe
import Data.List
import Data.Time.Clock

type Map = Map.HashMap


---------------------------------------------------------------------
-- UTILITY TYPES

newtype Step = Step Word32 deriving (Eq,Ord,Show,Binary,NFData,Hashable,Typeable)

incStep (Step i) = Step $ i + 1


type Duration = Double -- duration in seconds

duration :: IO a -> IO (Duration, a)
duration act = do
    start <- getCurrentTime
    res <- act
    end <- getCurrentTime
    return (fromRational $ toRational $ end `diffUTCTime` start, res)


type Time = Double -- how far you are through this run, in seconds

-- | Call once at the start, then call repeatedly to get Time values out
startTime :: IO (IO Time)
startTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ fromRational $ toRational $ end `diffUTCTime` start


---------------------------------------------------------------------
-- CALL STACK

newtype Stack = Stack [Key]

showStack :: Database -> Stack -> IO [String]
showStack db (Stack xs) = return $ reverse $ map show xs

addStack :: Key -> Stack -> Stack
addStack x (Stack xs) = Stack $ x : xs

checkStack :: [Key] -> Stack -> IO ()
checkStack new (Stack old)
    | bad:_ <- old `intersect` new = error $ "Invalid rules, recursion detected when trying to build: " ++ show bad
    | otherwise = return ()

emptyStack :: Stack
emptyStack = Stack []


---------------------------------------------------------------------
-- CENTRAL TYPES

type Trace = (String, Time, Time)


-- | Invariant: The database does not have any cycles when a Key depends on itself
data Database = Database
    {lock :: Lock
    ,status :: IORef (Map Key Status)
    ,step :: Step
    ,journal :: Update Key Result -> IO ()
    ,logger :: String -> IO () -- logging function
    }

data Status
    = Ready Result -- I have a value
    | Error SomeException -- I have been run and raised an error
    | Loaded Result -- Loaded from the database
    | Waiting Pending (Maybe Result) -- Currently checking if I am valid or building
      deriving Show

-- FIXME: Probably want Step's to be strict and unpacked? Benchmark on a large example
data Result = Result
    {result :: Value -- the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- the step for deciding if it's valid
    ,depends :: [[Key]] -- dependencies
    ,execution :: {-# UNPACK #-} !Duration -- how long it took when it was last run (seconds)
    ,traces :: [Trace] -- a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving Show


newtype Pending = Pending (IORef (IO ()))
    -- you must run this action when you finish, while holding DB lock
    -- after you have set the result to Error or Ready

instance Show Pending where show _ = "Pending"


isError Error{} = True; isError _ = False
isWaiting Waiting{} = True; isWaiting _ = False
isReady Ready{} = True; isReady _ = False


-- All the waiting operations are only valid when isWaiting
type Waiting = Status

afterWaiting :: Waiting -> IO () -> IO ()
afterWaiting (Waiting (Pending p) _) act = modifyIORef p (>> act)

newWaiting :: Maybe Result -> IO Waiting
newWaiting r = do ref <- newIORef $ return (); return $ Waiting (Pending ref) r

runWaiting :: Waiting -> IO ()
runWaiting (Waiting (Pending p) _) = join $ readIORef p

-- Wait for a set of actions to complete
-- If the action returns True, the function will not be called again
-- If the first argument is True, the thing is ended
waitFor :: [(a, Waiting)] -> (Bool -> a -> IO Bool) -> IO ()
waitFor ws@(_:_) act = do
    todo <- newIORef $ length ws
    forM_ ws $ \(k,w) -> afterWaiting w $ do
        t <- readIORef todo
        when (t /= 0) $ do
            b <- act (t == 1) k
            writeIORef todo $ if b then 0 else t - 1


getResult :: Status -> Maybe Result
getResult (Ready r) = Just r
getResult (Loaded r) = Just r
getResult (Waiting _ r) = r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

data Ops = Ops
    {valid :: Key -> Value -> IO Bool
        -- ^ Given a Key and a Value from the database, check it still matches the value stored on disk
    ,exec :: Stack -> Key -> IO (Either SomeException (Value, [[Key]], Duration, [Trace]))
        -- ^ Given a chunk of stack (bottom element first), and a key, either raise an exception or successfully build it
    }


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> Ops -> Stack -> [Key] -> IO (Either SomeException (Duration,[Value]))
build pool Database{..} Ops{..} stack ks = do
    checkStack ks stack
    join $ withLock lock $ do
        vs <- mapM (reduce stack) ks
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ return $ Right (0, [result r | Ready r <- vs])
         else if not $ null errs then
            return $ return $ Left $ head errs
         else do
            wait <- newBarrier
            waitFor (filter (isWaiting . snd) $ zip ks vs) $ \finish k -> do
                s <- readIORef status
                let done x = do signalBarrier wait x; return True
                case Map.lookup k s of
                    Just (Error e) -> done $ Left e
                    Just Ready{} | finish -> done $ Right [result r | k <- ks, let Ready r = fromJust $ Map.lookup k s]
                                 | otherwise -> return False
            return $ do
                (dur,res) <- duration $ blockPool pool $ waitBarrier wait
                return $ case res of
                    Left e -> Left e
                    Right v -> Right (dur,v)
    where
        k #= v = do
            s <- readIORef status
            writeIORef status $ Map.insert k v s
            let shw = head . words . show
            logger $ maybe "Missing" shw (Map.lookup k s) ++ " -> " ++ shw v ++ ", " ++ show k
            return v

        atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s

        -- Rules for each eval* function
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must not return Loaded

        reduce :: Stack -> Key -> IO Status
        reduce stack k = do
            s <- readIORef status
            case Map.lookup k s of
                Nothing -> run stack k Nothing
                Just (Loaded r) -> do
                    b <- valid k $ result r
                    logger $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)
                    if not b then run stack k $ Just r else check stack k r (depends r)
                Just res -> return res

        run :: Stack -> Key -> Maybe Result -> IO Waiting
        run stack k r = do
            w <- newWaiting r
            addPool pool $ do
                res <- exec (addStack k stack) k
                ans <- withLock lock $ do
                    ans <- k #= case res of
                        Left err -> Error err
                        Right (v,depends,execution,traces) ->
                            let c | Just r <- r, result r == v = changed r
                                  | otherwise = step
                            in Ready Result{result=v,changed=c,built=step,..}
                    runWaiting w
                    return ans
                case ans of
                    Ready r -> do
                        logger $ "result " ++ atom k ++ " = " ++ atom (result r)
                        journal $ Insert k r -- leave the DB lock before appending
                    Error _ -> do
                        logger $ "result " ++ atom k ++ " = error"
                        journal $ Delete k
                    _ -> return ()
            k #= w

        check :: Stack -> Key -> Result -> [[Key]] -> IO Status
        check stack k r [] =
            k #= Ready r
        check stack k r (ds:rest) = do
            vs <- mapM (reduce (addStack k stack)) ds
            let ws = filter (isWaiting . snd) $ zip ds vs
            if any isError vs || any (> built r) [changed | Ready Result{..} <- vs] then
                run stack k $ Just r
             else if null ws then
                check stack k r rest
             else do
                self <- newWaiting $ Just r
                waitFor ws $ \finish d -> do
                    s <- readIORef status
                    let buildIt = do
                            b <- run stack k $ Just r
                            afterWaiting b $ runWaiting self
                            return True
                    case Map.lookup d s of
                        Just Error{} -> buildIt
                        Just (Ready r2)
                            | changed r2 > built r -> buildIt
                            | finish -> do
                                res <- check stack k r rest
                                if not $ isWaiting res
                                    then runWaiting self
                                    else afterWaiting res $ runWaiting self
                                return True
                            | otherwise -> return False
                k #= self


---------------------------------------------------------------------
-- QUERY DATABASE

-- | Return a list of keys in an order which would build them bottom up. Relies on the invariant
--   that the database is not cyclic.
allEntries :: Database -> IO [(Key,Value)]
allEntries Database{..} = do
    status <- readIORef status
    return $ ordering [((k, result i), concat $ depends i) | (k,v) <- Map.toList status, Just i <- [getResult v]]
    where
        ordering :: Eq a => [((a,b), [a])] -> [(a,b)]
        ordering xs = f [(a, nub b `intersect` as) | let as = map (fst . fst) xs, (a,b) <- xs]
            where
                f xs | null xs = []
                     | null now = error "Internal invariant broken, database seems to be cyclic (probably during lint)"
                     | otherwise = let ns = map fst now in ns ++ f [(a,b \\ map fst ns) | (a,b) <- later]
                    where (now,later) = partition (null . snd) xs


showJSON :: Database -> IO String
showJSON Database{..} = do
    status <- readIORef status
    let ids = Map.fromList $ zip (Map.keys status) [0..]
        f (k, v) | Just Result{..} <- getResult v =
            let xs = ["name:" ++ show (show k)
                     ,"built:" ++ showStep built
                     ,"changed:" ++ showStep changed
                     ,"depends:" ++ show (mapMaybe (`Map.lookup` ids) (concat depends))
                     ,"execution:" ++ show execution] ++
                     ["traces:[" ++ intercalate "," (map showTrace traces) ++ "]" | traces /= []]
                showStep (Step i) = show i
                showTrace (a,b,c) = "{start:" ++ show b ++ ",stop:" ++ show c ++ ",command:" ++ show a ++ "}"
            in  ["{" ++ intercalate ", " xs ++ "}"]
        f _ = []
    return $ "[" ++ intercalate "\n," (concatMap f $ Map.toList status) ++ "\n]"


checkValid :: Database -> (Key -> Value -> IO Bool) -> IO ()
checkValid Database{..} valid = do
    status <- readIORef status
    logger "Starting validity/lint checking"
    bad <- fmap concat $ forM (Map.toList status) $ \(k,v) -> case v of
        Ready r -> do
            good <- valid k (result r)
            logger $ "Checking if " ++ show k ++ " is " ++ show (result r) ++ ", " ++ if good then "passed" else "FAILED"
            return [show k ++ " is no longer " ++ show (result r) | not good && not (special k)]
        _ -> return []
    if null bad
        then logger "Validity/lint check passed"
        else error $ unlines $ "Error: Dependencies have changed since being built:" : bad

    where
        -- special case for these things, since the purpose is to break the invariant
        special k = s == "AlwaysRun" || "Oracle " `isPrefixOf` s
            where s = show k


---------------------------------------------------------------------
-- STORAGE

-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> Result
toStepResult i = Result (newValue i) i i [] 0 []

fromStepResult :: Result -> Step
fromStepResult = fromValue . result


withDatabase :: (String -> IO ()) -> FilePath -> Int -> (Database -> IO a) -> IO a
withDatabase logger filename version act = do
    registerWitness $ StepKey ()
    registerWitness $ Step 0
    witness <- currentWitness
    withStorage logger filename version witness $ \mp journal -> do
        status <- newIORef $ Map.map Loaded mp
        let step = maybe (Step 1) (incStep . fromStepResult) $ Map.lookup stepKey mp
        journal $ Insert stepKey $ toStepResult step
        lock <- newLock
        act Database{..}


instance BinaryWith Witness Step where
    putWith _ x = put x
    getWith _ = get

instance BinaryWith Witness Result where
    putWith ws (Result x1 x2 x3 x4 x5 x6) = putWith ws x1 >> put x2 >> put x3 >> putWith ws x4 >> put x5 >> put x6
    getWith ws = do x1 <- getWith ws; x2 <- get; x3 <- get; x4 <- getWith ws; x5 <- get; x6 <- get; return $ Result x1 x2 x3 x4 x5 x6
