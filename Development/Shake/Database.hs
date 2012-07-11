{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Development.Shake.Database(
    Time, startTime, Duration, duration, Trace,
    Database, withDatabase,
    Ops(..), build, Depends,
    Stack, emptyStack, showStack,
    showJSON, checkValid,
    ) where

import Development.Shake.Binary
import Development.Shake.Pool
import Development.Shake.Value
import Development.Shake.Locks
import Development.Shake.Storage
import Development.Shake.Intern as Intern

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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing f = return ()


---------------------------------------------------------------------
-- CALL STACK

newtype Stack = Stack [Id]

showStack :: Database -> Stack -> IO [String]
showStack Database{..} (Stack xs) = do
    status <- withLock lock $ readIORef status
    return $ reverse $ map (maybe "<unknown>" (show . fst) . flip Map.lookup status) xs

addStack :: Id -> Stack -> Stack
addStack x (Stack xs) = Stack $ x : xs

checkStack :: [Id] -> Stack -> Maybe Id
checkStack new (Stack old)
    | bad:_ <- old `intersect` new = Just bad
    | otherwise = Nothing

emptyStack :: Stack
emptyStack = Stack []


---------------------------------------------------------------------
-- CENTRAL TYPES

type Trace = (String, Time, Time)


-- | Invariant: The database does not have any cycles when a Key depends on itself
data Database = Database
    {lock :: Lock
    ,intern :: IORef (Intern Key)
    ,status :: IORef (Map Id (Key, Status))
    ,step :: Step
    ,journal :: Id -> (Key, Status {- Loaded or Missing -}) -> IO ()
    ,logger :: String -> IO () -- logging function
    }

data Status
    = Ready Result -- I have a value
    | Error SomeException -- I have been run and raised an error
    | Loaded Result -- Loaded from the database
    | Waiting Pending (Maybe Result) -- Currently checking if I am valid or building
    | Missing -- I am only here because I got into the Intern table
      deriving Show

-- FIXME: Probably want Step's to be strict and unpacked? Benchmark on a large example
data Result = Result
    {result :: Value -- the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- the step for deciding if it's valid
    ,depends :: [[Id]] -- dependencies
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

newtype Depends = Depends {fromDepends :: [Id]}
    deriving (NFData)

data Ops = Ops
    {valid :: Key -> Value -> IO Bool
        -- ^ Given a Key and a Value from the database, check it still matches the value stored on disk
    ,exec :: Stack -> Key -> IO (Either SomeException (Value, [Depends], Duration, [Trace]))
        -- ^ Given a chunk of stack (bottom element first), and a key, either raise an exception or successfully build it
    }


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Pool -> Database -> Ops -> Stack -> [Key] -> IO (Either SomeException (Duration,Depends,[Value]))
build pool Database{..} Ops{..} stack ks = do
    join $ withLock lock $ do
        is <- forM ks $ \k -> do
            is <- readIORef intern
            case Intern.lookup k is of
                Just i -> return i
                Nothing -> do
                    (is, i) <- return $ Intern.add k is
                    writeIORef intern is
                    modifyIORef status $ Map.insert i (k,Missing)
                    return i

        whenJust (checkStack is stack) $ \bad -> do
            status <- readIORef status
            error $ "Invalid rules, recursion detected when trying to build: " ++ maybe "<unknown>" (show . fst) (Map.lookup bad status)

        vs <- mapM (reduce stack) is
        let errs = [e | Error e <- vs]
        if all isReady vs then
            return $ return $ Right (0, Depends is, [result r | Ready r <- vs])
         else if not $ null errs then
            return $ return $ Left $ head errs
         else do
            wait <- newBarrier
            waitFor (filter (isWaiting . snd) $ zip is vs) $ \finish i -> do
                s <- readIORef status
                let done x = do signalBarrier wait x; return True
                case Map.lookup i s of
                    Just (_, Error e) -> done $ Left e
                    Just (_, Ready{}) | finish -> done $ Right [result r | i <- is, let Ready r = snd $ fromJust $ Map.lookup i s]
                                      | otherwise -> return False
            return $ do
                (dur,res) <- duration $ blockPool pool $ waitBarrier wait
                return $ case res of
                    Left e -> Left e
                    Right v -> Right (dur,Depends is,v)
    where
        (#=) :: Id -> (Key, Status) -> IO Status
        i #= (k,v) = do
            s <- readIORef status
            writeIORef status $ Map.insert i (k,v) s
            let shw = head . words . show
            logger $ maybe "Missing" (shw . snd) (Map.lookup i s) ++ " -> " ++ shw v ++ ", " ++ maybe "<unknown>" (show . fst) (Map.lookup i s)
            return v

        atom x = let s = show x in if ' ' `elem` s then "(" ++ s ++ ")" else s

        -- Rules for each eval* function
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must not return Loaded

        reduce :: Stack -> Id -> IO Status
        reduce stack i = do
            s <- readIORef status
            case Map.lookup i s of
                Nothing -> error $ "Shake internal error: interned value " ++ show i ++ " is missing from the database"
                Just (k, Missing) -> run stack i k Nothing
                Just (k, Loaded r) -> do
                    b <- valid k $ result r
                    logger $ "valid " ++ show b ++ " for " ++ atom k ++ " " ++ atom (result r)
                    if not b then run stack i k $ Just r else check stack i k r (depends r)
                Just (k, res) -> return res

        run :: Stack -> Id -> Key -> Maybe Result -> IO Waiting
        run stack i k r = do
            w <- newWaiting r
            addPool pool $ do
                res <- exec (addStack i stack) k
                ans <- withLock lock $ do
                    ans <- i #= (k, case res of
                        Left err -> Error err
                        Right (v,deps,execution,traces) ->
                            let c | Just r <- r, result r == v = changed r
                                  | otherwise = step
                            in Ready Result{result=v,changed=c,built=step,depends=map fromDepends deps,..})
                    runWaiting w
                    return ans
                case ans of
                    Ready r -> do
                        logger $ "result " ++ atom k ++ " = " ++ atom (result r)
                        journal i (k, Loaded r) -- leave the DB lock before appending
                    Error _ -> do
                        logger $ "result " ++ atom k ++ " = error"
                        journal i (k, Missing)
                    _ -> return ()
            i #= (k, w)

        check :: Stack -> Id -> Key -> Result -> [[Id]] -> IO Status
        check stack i k r [] =
            i #= (k, Ready r)
        check stack i k r (ds:rest) = do
            vs <- mapM (reduce (addStack i stack)) ds
            let ws = filter (isWaiting . snd) $ zip ds vs
            if any isError vs || any (> built r) [changed | Ready Result{..} <- vs] then
                run stack i k $ Just r
             else if null ws then
                check stack i k r rest
             else do
                self <- newWaiting $ Just r
                waitFor ws $ \finish d -> do
                    s <- readIORef status
                    let buildIt = do
                            b <- run stack i k $ Just r
                            afterWaiting b $ runWaiting self
                            return True
                    case Map.lookup d s of
                        Just (_, Error{}) -> buildIt
                        Just (_, Ready r2)
                            | changed r2 > built r -> buildIt
                            | finish -> do
                                res <- check stack i k r rest
                                if not $ isWaiting res
                                    then runWaiting self
                                    else afterWaiting res $ runWaiting self
                                return True
                            | otherwise -> return False
                i #= (k, self)


---------------------------------------------------------------------
-- QUERY DATABASE

-- | Given a map of representing a dependency order (with a show for error messages), find an ordering for the items such
--   that no item points to an item before itself.
--   Raise an error if you end up with a cycle.
dependencyOrder :: (Eq a, Hashable a) => (a -> String) -> Map a [a] -> [a]
-- Algorithm:
--    Divide everyone up into those who have no dependencies [Id]
--    And those who depend on a particular Id, Dep :-> Maybe [(Key,[Dep])]
--    Where d :-> Just (k, ds), k depends on firstly d, then remaining on ds
--    For each with no dependencies, add to list, then take its dep hole and
--    promote them either to Nothing (if ds == []) or into a new slot.
--    k :-> Nothing means the key has already been freed
dependencyOrder shw status = f (map fst noDeps) $ Map.map Just $ Map.fromListWith (++) [(d, [(k,ds)]) | (k,d:ds) <- hasDeps]
    where
        (noDeps, hasDeps) = partition (null . snd) $ Map.toList status

        f [] mp | null bad = []
                | otherwise = error $ unlines $
                    "Internal invariant broken, database seems to be cyclic" :
                    map ("    " ++) bad ++
                    ["... plus " ++ show (length badOverflow) ++ " more ..." | not $ null badOverflow]
            where (bad,badOverflow) = splitAt 10 $ [shw i | (i, Just _) <- Map.toList mp]

        f (x:xs) mp = x : f (now++xs) later
            where Just free = Map.lookupDefault (Just []) x mp
                  (now,later) = foldl' g ([], Map.insert x Nothing mp) free

        g (free, mp) (k, []) = (k:free, mp)
        g (free, mp) (k, d:ds) = case Map.lookupDefault (Just []) d mp of
            Nothing -> g (free, mp) (k, ds)
            Just todo -> (free, Map.insert d (Just $ (k,ds) : todo) mp)


showJSON :: Database -> IO String
showJSON Database{..} = do
    status <- readIORef status
    let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
        order = dependencyOrder shw $ Map.map (maybe [] (concat . depends) . getResult . snd) status
        ids = Map.fromList $ zip order [0..]

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
    return $ "[" ++ intercalate "\n," (concat [maybe (error "Internal error in showJSON") f $ Map.lookup i status | i <- order]) ++ "\n]"


checkValid :: Database -> (Key -> Value -> IO Bool) -> IO ()
checkValid Database{..} valid = do
    status <- readIORef status
    logger "Starting validity/lint checking"
    bad <- fmap concat $ forM (Map.toList status) $ \(i,v) -> case v of
        (key, Ready Result{..}) -> do
            good <- valid key result
            logger $ "Checking if " ++ show key ++ " is " ++ show result ++ ", " ++ if good then "passed" else "FAILED"
            return [show key ++ " is no longer " ++ show result | not good && not (special key)]
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
    withStorage logger filename version witness $ \mp2 journal -> do
        let mp1 = Intern.fromList [(k, i) | (i, (k,_)) <- Map.toList mp2]

        (mp1, stepId) <- case Intern.lookup stepKey mp1 of
            Just stepId -> return (mp1, stepId)
            Nothing -> do
                (mp1, stepId) <- return $ Intern.add stepKey mp1
                return (mp1, stepId)

        intern <- newIORef mp1
        status <- newIORef mp2
        let step = case Map.lookup stepId mp2 of
                        Just (_, Loaded r) -> incStep $ fromStepResult r
                        _ -> Step 1
        journal stepId $ (stepKey, Loaded $ toStepResult step)
        lock <- newLock
        act Database{..}


instance BinaryWith Witness Step where
    putWith _ x = put x
    getWith _ = get

instance BinaryWith Witness Result where
    putWith ws (Result x1 x2 x3 x4 x5 x6) = putWith ws x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6
    getWith ws = do x1 <- getWith ws; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; return $ Result x1 x2 x3 x4 x5 x6

instance BinaryWith Witness Status where
    putWith ctx Missing = putWord8 0
    putWith ctx (Loaded x) = putWord8 1 >> putWith ctx x
    putWith ctx x = error $ "putWith: Cannot write Status with constructor " ++ head (words $ show x)
    getWith ctx = do i <- getWord8; if i == 0 then return Missing else fmap Loaded $ getWith ctx
