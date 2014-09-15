{-# LANGUAGE PatternGuards #-}

module Test.Random(main) where

import Development.Shake
import Test.Type
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import System.Environment
import System.Exit
import System.Random
import General.Base


inputRange = [1..10]

data Value = Single Int | Multiple [[Value]]
    deriving (Read,Show,Eq)

data Source = Input Int | Output Int | Bang
    deriving (Read,Show)

data Logic = Logic Int [[Source]]
           | Want [Int]
    deriving (Read,Show)


main = shaken test $ \args obj -> do
    let toFile (Input i) = obj $ "input-" ++ show i ++ ".txt"
        toFile (Output i) = obj $ "output-" ++ show i ++ ".txt"
        toFile Bang = error "BANG"

    let randomSleep = liftIO $ do
            i <- randomRIO (0, 25)
            sleep $ doubleToFloat $ intToDouble i / 100

    forM_ (map read $ filter (isNothing . asDuration) args) $ \x -> case x of
        Want xs -> want $ map (toFile . Output) xs
        Logic out srcs -> toFile (Output out) *> \out -> do
            res <- fmap (show . Multiple) $ forM srcs $ \src -> do
                randomSleep
                need $ map toFile src
                mapM (liftIO . fmap read . readFileStrict . toFile) src
            randomSleep
            writeFileChanged out res


asDuration :: String -> Maybe Double
asDuration x
    | "s" `isSuffixOf` x, [(i,"")] <- reads $ init x = Just i
    | "m" `isSuffixOf` x, [(i,"")] <- reads $ init x = Just $ i * 60
    | otherwise = Nothing


test build obj = do
    limit <- do
        args <- getArgs
        let bound = listToMaybe $ reverse $ mapMaybe asDuration args
        start <- getCurrentTime
        return $ when (isJust bound) $ do
            now <- getCurrentTime
            when (fromRational (toRational $ now `diffUTCTime` start) > fromJust bound) exitSuccess

    forM_ [1..] $ \count -> do
        limit
        putStrLn $ "* PERFORMING RANDOM TEST " ++ show count
        build ["clean"]
        build [] -- to create the directory
        forM_ inputRange $ \i ->
            writeFile (obj $ "input-" ++ show i ++ ".txt") $ show $ Single i
        logic <- randomLogic
        runLogic [] logic
        chng <- filterM (const randomIO) inputRange   
        forM_ chng $ \i ->
            writeFile (obj $ "input-" ++ show i ++ ".txt") $ show $ Single $ negate i
        runLogic chng logic
        forM_ inputRange $ \i ->
            writeFile (obj $ "input-" ++ show i ++ ".txt") $ show $ Single i
        logicBang <- addBang =<< addBang logic
        j <- randomRIO (1::Int,8)
        res <- try $ build $ "--exception" : ("-j" ++ show j) : map show (logicBang ++ [Want [i | Logic i _ <- logicBang]])
        case res of
            Left err
                | "BANG" `isInfixOf` show (err :: SomeException) -> return () -- error I expected
                | otherwise -> error $ "UNEXPECTED ERROR: " ++ show err
            _ -> return () -- occasionally we only put BANG in places with no dependenies that don't get rebuilt
        runLogic [] $ logic ++ [Want [i | Logic i _ <- logic]]
        where
            runLogic :: [Int] -> [Logic] -> IO ()
            runLogic negated xs = do
                let poss = [i | Logic i _ <- xs]
                i <- randomRIO (0, 7)
                wants <- replicateM i $ do
                    i <- randomRIO (0, 5)
                    replicateM i $ randomElem poss
                sleepFileTime
                j <- randomRIO (1::Int,8)
                build $ ("-j" ++ show j) : map show (xs ++ map Want wants)

                let value i = case [ys | Logic j ys <- xs, j == i] of
                        [ys] -> Multiple $ flip map ys $ map $ \i -> case i of
                            Input i -> Single $ if i `elem` negated then negate i else i
                            Output i -> value i
                forM_ (concat wants) $ \i -> do
                    let wanted = value i
                    got <- fmap read $ readFileStrict $ obj $ "output-" ++ show i ++ ".txt"
                    when (wanted /= got) $
                        error $ "INCORRECT VALUE for " ++ show i


addBang :: [Logic] -> IO [Logic]
addBang xs = do
    i <- randomRIO (0, length xs - 1)
    let (before,now:after) = splitAt i xs
    now <- f now
    return $ before ++ now : after
    where
        f (Logic log xs) = do
            i <- randomRIO (0, length xs)
            let (before,after) = splitAt i xs
            return $ Logic log $ before ++ [Bang] : after


randomLogic :: IO [Logic] -- only Logic constructors
randomLogic = do
    rules <- randomRIO (1,100)
    f rules $ map Input inputRange
    where
        f 0 avail = return []
        f i avail = do
            needs <- randomRIO (0,3)
            xs <- replicateM needs $ do
                ns <- randomRIO (0,3)
                replicateM ns $ randomElem avail
            let r = Logic i xs
            fmap (r:) $ f (i-1) (Output i:avail)
