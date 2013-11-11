
module Development.Shake.Task(
    TaskM, runTaskM, Task, kill, wait, spawn, pause
    ) where

import Control.Monad.IO.Class
import Control.Concurrent
import Development.Shake.General
import Data.Maybe
import Data.List


---------------------------------------------------------------------
-- UNDERLYING JOB MECHANISM

data Tasks = Tasks (Var S)

type TaskId = Int

data S = S
    {taskId :: TaskId
    ,available :: Int
    ,priority :: [IO ()] -- things which should run now with top priority
    ,queued :: [([Task], IO ())] -- things which should run when they have no queued tasks
    ,retask :: [(Task, Maybe Task)] -- things which completed giving a forwarding task
    }


newTasks :: Int -> IO Tasks
newTasks p = fmap Tasks $ newVar $ S 0 p [] [] []


data Task = Task TaskId
          | Killer
            deriving Eq


rejig :: Tasks -> (S -> S) -> IO ()
rejig tasks@(Tasks s) user = modifyVar_ s (f . user)
    where
        f s = do
            s <- return $ applyRetask s
            s <- return $ makePriority s
            (s, act) <- return $ grabTask s
            case act of
                Nothing -> return s
                Just a -> do
                    forkIO $ do
                        a
                        rejig tasks $ \s -> s{available = available s + 1}
                    f $ s{available = available s - 1} -- continue, since you may have availability left

        applyRetask s = s{queued = [(mapMaybe g ts, act) | (ts, act) <- queued s]}
            where g t = fromMaybe (Just t) $ lookup t $ retask s

        makePriority s = s{priority = map snd bad ++ priority s, queued = rest}
            where (bad,rest) = partition (elem Killer . fst) $ queued s

        grabTask s | available s <= 0 = (s, Nothing)
                   | x:xs <- priority s = (s{priority=xs}, Just x)
                   | (x:xs, rest) <- partition (null . fst) $ queued s = (s{queued = xs++rest}, Just $ snd x)
                   | otherwise = (s, Nothing)


newTaskId :: Tasks -> IO Task
newTaskId (Tasks s) = modifyVar s $ \s -> return (s{taskId = taskId s + 1}, Task $ taskId s)


newTask
    :: Tasks
    -> [Task] -- ^ Tasks to wait for before starting
    -> IO (Maybe Task) -- ^ Action, with tasks to wait for before finishing
    -> IO Task -- ^ This task
newTask tasks wait act = do
    t <- newTaskId tasks
    let op = do res <- act; rejig tasks $ \s -> s{retask = (t, res) : retask s}
    rejig tasks $ \s -> s{queued = (wait, op) : queued s}
    return t


-- Task, plus action to say it is done
pauseTask :: Tasks -> IO (Task, IO ())
pauseTask tasks = do
    t <- newTaskId tasks
    return $ (,) t $ rejig tasks $ \s -> s{retask = (t,Nothing) : retask s}


-- If something depends on priority then it gets run with top priority
killTask :: Tasks -> IO Task
killTask _ = return Killer


runTask :: Tasks -> Task -> IO ()
runTask tasks t = do
    bar <- newBarrier
    newTask tasks [t] (signalBarrier bar () >> return Nothing)
    waitBarrier bar


---------------------------------------------------------------------
-- TASK MONAD

-- Basically a ReaderT of Tasks, plus a Pipe-like Step
data TaskM a = TaskM {taskM :: Tasks -> IO (Step a)}

data Step a = Return a
            | Wait [Task] (TaskM a)
            | Kill

instance Functor Step where
    fmap f (Return x) = Return $ f x
    fmap f (Wait t x) = Wait t $ fmap f x
    fmap f Kill = Kill

instance Functor TaskM where
    fmap f (TaskM x) = TaskM $ \tasks -> fmap (fmap f) $ x tasks

instance MonadIO TaskM where
    liftIO x = TaskM $ const $ fmap Return x

instance Monad TaskM where
    return a = TaskM $ const $ return $ Return a
    a >>= f = TaskM $ \tasks -> do
        a <- taskM a tasks
        case a of
            Kill -> return Kill
            Return x -> taskM (f x) tasks
            Wait ts o -> return $ Wait ts $ o >>= f

toTask :: Tasks -> TaskM () -> IO Task
toTask tasks x = do
    x <- taskM x tasks
    case x of
        Kill -> killTask tasks
        Return a -> newTask tasks [] (return Nothing)
        Wait ts x -> newTask tasks ts $ fmap Just $ toTask tasks x

runTaskM :: Int -> TaskM () -> IO ()
runTaskM p x = do
    tasks <- newTasks p
    toTask tasks x >>= runTask tasks

getTasks :: TaskM Tasks
getTasks = TaskM $ \tasks -> return $ Return tasks

spawn :: TaskM () -> TaskM Task
spawn t = do
    tasks <- getTasks
    liftIO $ toTask tasks t

wait :: [Task] -> TaskM ()
wait xs = TaskM $ const $ return $ Wait xs $ return ()

kill :: TaskM ()
kill = TaskM $ const $ return Kill

pause :: (IO () -> IO ()) -> TaskM ()
pause with = do
    tasks <- getTasks
    (job, act) <- liftIO $ pauseTask tasks
    liftIO $ with act
    wait [job]
