{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables, NamedFieldPuns, GADTs #-}
{-# LANGUAGE Rank2Types, ConstraintKinds, TupleSections #-}

module Development.Shake.Internal.Core.Build(
    getDatabaseValue,
    historyIsEnabled, historySave, historyLoad,
    apply, apply1,
    ) where

import Development.Shake.Classes
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.History
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Wait3
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import General.Extra
import qualified General.Intern as Intern
import General.Intern(Id)

import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Numeric.Extra
import qualified Data.HashMap.Strict as Map
import qualified General.Ids as Ids
import Development.Shake.Internal.Core.Rules
import Data.Typeable.Extra
import Data.IORef.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Either.Extra
import System.Time.Extra
import Prelude


---------------------------------------------------------------------
-- LOW-LEVEL OPERATIONS ON THE DATABASE

getKeyId :: Database -> Key -> Locked Id
getKeyId Database{..} k = liftIO $ do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            -- make sure to write it into Status first to maintain Database invariants
            Ids.insert status i (k,Missing)
            writeIORef' intern is
            return i

getIdKeyStatus :: Database -> Id -> Locked (Key, Status)
getIdKeyStatus Database{..} i = liftIO $ do
    res <- Ids.lookup status i
    case res of
        Nothing -> throwM $ errorInternal $ "interned value missing from database, " ++ show i
        Just v -> return v


setIdKeyStatus :: Global -> Database -> Id -> Key -> Status -> Locked ()
setIdKeyStatus Global{..} database@Database{..} i k v = do
    liftIO $ globalDiagnostic $ do
        old <- Ids.lookup status i
        let changeStatus = maybe "Missing" (statusType . snd) old ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) old
        let changeValue = case v of
                Ready r -> Just $ "    = " ++ showBracket (result r) ++ " " ++ (if built r == changed r then "(changed)" else "(unchanged)")
                _ -> Nothing
        return $ changeStatus ++ maybe "" ("\n" ++) changeValue
    setIdKeyStatusQuiet database i k v

setIdKeyStatusQuiet :: Database -> Id -> Key -> Status -> Locked ()
setIdKeyStatusQuiet Database{..} i k v =
    liftIO $ Ids.insert status i (k,v)


---------------------------------------------------------------------
-- QUERIES

getDatabaseValue :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action (Maybe (Either BS.ByteString value))
getDatabaseValue k = do
    Global{..} <- Action getRO
    (_, status) <- liftIO $ runLocked globalDatabase $ \database ->
        getIdKeyStatus database =<< getKeyId database (newKey k)
    return $ case getResult status of
        Just r -> Just $ fromValue <$> result r
        _ -> Nothing


---------------------------------------------------------------------
-- NEW STYLE PRIMITIVES

-- | Lookup the value for a single Id, may need to spawn it
lookupOne :: Global -> Stack -> Database -> Id -> Locked (Wait (Either SomeException (Result Value)))
lookupOne global stack database i = do
    (k, s) <- getIdKeyStatus database i
    case s of
        Ready r -> return $ Now $ Right r
        Error e -> return $ Now $ Left e
        Running{} | Left e <- addStack i k stack -> return $ Now $ Left e
        _ -> return $ Later $ \continue -> do
            (_, s) <- getIdKeyStatus database i
            case s of
                Ready r -> continue $ Right r
                Error e -> continue $ Left e
                Running (NoShow w) r -> do
                    let w2 v = w v >> continue v
                    setIdKeyStatusQuiet database i k $ Running (NoShow w2) r
                Loaded r -> do wait <- buildOne global stack database i k (Just r); fromLater wait continue
                Missing -> do wait <- buildOne global stack database i k Nothing; fromLater wait continue


-- | Build a key, must currently be either Loaded or Missing, changes to Waiting
buildOne :: Global -> Stack -> Database -> Id -> Key -> Maybe (Result BS.ByteString) -> Locked (Wait (Either SomeException (Result Value)))
buildOne global@Global{..} stack database i k r = case addStack i k stack of
    Left e -> do
        setIdKeyStatus global database i k $ Error e
        return $ Now $ Left e
    Right stack -> return $ Later $ \continue -> do
        setIdKeyStatus global database i k (Running (NoShow continue) r)
        go <- buildRunMode global stack database k r
        fromLater go $ \mode -> liftIO $ addPool PoolStart globalPool $
            runKey global stack k r mode $ \res -> do
                runLocked globalDatabase $ \_ -> do
                    let val = fmap runValue res
                    res <- getIdKeyStatus database i
                    w <- case snd res of
                        Running (NoShow w) _ -> return w
                        s -> throwM $ errorInternal $ "expected Waiting but got " ++ statusType s ++ ", key " ++ show k
                    setIdKeyStatus global database i k $ either Error Ready val
                    w val
                case res of
                    Right RunResult{..} | runChanged /= ChangedNothing -> journal database i k runValue{result=runStore}
                    _ -> return ()


-- | Compute the value for a given RunMode and a restore function to run
buildRunMode :: Global -> Stack -> Database -> Key -> Maybe (Result a) -> Locked (Wait RunMode)
buildRunMode global stack database k me = return $ Later $ \continue -> do
    changed <- case me of
        Nothing -> return $ Now True
        Just me -> buildRunDependenciesChanged global stack database me
    fromLater changed $ \changed -> continue $ if changed then RunDependenciesChanged else RunDependenciesSame


-- | Have the dependencies changed
buildRunDependenciesChanged :: Global -> Stack -> Database -> Result a -> Locked (Wait Bool)
buildRunDependenciesChanged global stack database me = fmap isJust <$> firstJustWaitOrdered
    [firstJustWaitUnordered $ map (fmap (fmap test) . lookupOne global stack database) x | Depends x <- depends me]
    where
        test (Right dep) | changed dep <= built me = Nothing
        test _ = Just ()


---------------------------------------------------------------------
-- ACTUAL WORKERS

applyKeyValue :: [String] -> [Key] -> Action [Value]
applyKeyValue callStack [] = return []
applyKeyValue callStack ks = do
    global@Global{..} <- Action getRO
    Local{localStack} <- Action getRW
    let stack = addCallStack (if shakeVerbosity globalOptions > Normal then callStack else take 1 callStack) localStack

    (is, wait) <- liftIO $ runLocked globalDatabase $ \database -> do
        is <- mapM (getKeyId database) ks
        wait <- firstJustWaitUnordered $ map (fmap (fmap (either Just (const Nothing))) . lookupOne global stack database) $ nubOrd is
        wait <- flip fmapWait (return wait) $ \x -> case x of
            Just e -> return $ Left e
            Nothing -> Right <$> mapM (fmap (\(_, Ready r) -> result r) . getIdKeyStatus database) is
        return (is, wait)
    Action $ modifyRW $ \s -> s{localDepends = Depends is : localDepends s}

    case wait of
        Now vs -> either throwM return vs
        Later k -> do
            offset <- liftIO offsetTime
            vs <- Action $ captureRAW $ \continue ->
                runLocked globalDatabase $ \_ -> k $ \x ->
                    liftIO $ addPool (if isLeft x then PoolException else PoolResume) globalPool $ continue x
            offset <- liftIO offset
            Action $ modifyRW $ addDiscount offset
            return vs


runKey
    :: Global
    -> Stack  -- Given the current stack with the key added on
    -> Key -- The key to build
    -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
    -> RunMode -- True if any of the children were dirty
    -> Capture (Either SomeException (RunResult (Result Value)))
        -- Either an error, or a (the produced files, the result).
runKey global@Global{globalOptions=ShakeOptions{..},..} stack k r mode continue = do
    let tk = typeKey k
    BuiltinRule{..} <- case Map.lookup tk globalRules of
        Nothing -> throwM $ errorNoRuleToBuildType tk (Just $ show k) Nothing
        Just r -> return r

    let s = (newLocal stack shakeVerbosity){localBuiltinVersion = builtinVersion}
    time <- offsetTime
    runAction global s (do
        res <- builtinRun k (fmap result r) mode
        liftIO $ evaluate $ rnf res

        -- completed, now track anything required afterwards
        lintTrackFinished
        producesCheck

        Action $ fmap ((,) res) getRW) $ \x -> case x of
            Left e -> do
                e <- if isNothing shakeLint then return e else handle return $
                    do lintCurrentDirectory globalCurDir $ "Running " ++ show k; return e
                continue . Left . toException =<< shakeException global stack e
            Right (RunResult{..}, Local{..})
                | runChanged == ChangedNothing || runChanged == ChangedStore, Just r <- r ->
                    continue $ Right $ RunResult runChanged runStore (r{result = runValue})
                | otherwise -> do
                    dur <- time
                    let (cr, c) | Just r <- r, runChanged == ChangedRecomputeSame = (ChangedRecomputeSame, changed r)
                                | otherwise = (ChangedRecomputeDiff, globalStep)
                    continue $ Right $ RunResult cr runStore Result
                        {result = runValue
                        ,changed = c
                        ,built = globalStep
                        ,depends = nubDepends $ reverse localDepends
                        ,execution = doubleToFloat $ dur - localDiscount
                        ,traces = reverse localTraces}


---------------------------------------------------------------------
-- USER key/value WRAPPERS

-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'addBuiltinRule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: (Partial, RuleResult key ~ value, ShakeValue key, Typeable value) => [key] -> Action [value]
-- Don't short-circuit [] as we still want error messages
apply ks = do
    -- this is the only place a user can inject a key into our world, so check they aren't throwing
    -- in unevaluated bottoms
    liftIO $ mapM_ (evaluate . rnf) ks

    let tk = typeRep ks
    Local{localBlockApply} <- Action getRW
    whenJust localBlockApply $ throwM . errorNoApply tk (show <$> listToMaybe ks)
    fmap (map fromValue) $ applyKeyValue callStackFull $ map newKey ks


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: (Partial, RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action value
apply1 = withFrozenCallStack $ fmap head . apply . return



---------------------------------------------------------------------
-- HISTORY STUFF

-- | Load a value from the history. Given a @key@, a version from any user rule
--   (or @\"\"@), return the payload that was stored by 'historySave'.
--
--   If this function returns 'Just' it will also have restored any files that
--   were saved by 'historySave'.
historyLoad :: ShakeValue k => k -> Int -> Action (Maybe BS.ByteString)
historyLoad k userVersion = do
    global@Global{..} <- Action getRO
    Local{localStack, localBuiltinVersion} <- Action getRW
    case globalHistory of
        Nothing -> return Nothing
        Just history -> do
            res <- liftIO $ runLocked globalDatabase $ \database -> do
                let ask k = do
                        i <- getKeyId database k
                        let identify = Just . runIdentify globalRules k . result
                        fmap (either (const Nothing) identify) <$> lookupOne global localStack database i
                res <- lookupHistory history ask (newKey k) localBuiltinVersion (Version userVersion)
                flip fmapWait (return res) $ \x -> case x of
                    Nothing -> return Nothing
                    Just (a,b,c) -> Just . (a,,c) <$> mapM (mapM $ getKeyId database) b
            res <- case res of
                Now x -> return x
                Later k -> do
                    offset <- liftIO offsetTime
                    res <- Action $ captureRAW $ \continue ->
                        runLocked globalDatabase $ \_ -> k $ \x ->
                            liftIO $ addPool PoolResume globalPool $ continue $ Right x
                    offset <- liftIO offset
                    Action $ modifyRW $ addDiscount offset
                    return res
            case res of
                Nothing -> return Nothing
                Just (res, deps, restore) -> do
                    liftIO $ globalDiagnostic $ return $ "History hit for " ++ show k
                    liftIO restore
                    Action $ modifyRW $ \s -> s{localDepends = reverse $ map Depends deps}
                    return (Just res)


-- | Is the history enabled.
historyIsEnabled :: Action Bool
historyIsEnabled = Action $
    (isJust . globalHistory <$> getRO) &&^ (localHistory <$> getRW)

-- | Save a value to the history. Record a @key@, the version of any user rule
--   (or @\"\"@), and a payload. Must be run at the end of the rule, after
--   any dependencies have been captured. If history is enabled, stores the information
--   in a cache.
--
--   This function relies on 'produces' to have been called correctly to describe
--   which files were written during the execution of this rule.
historySave :: ShakeValue k => k -> Int -> BS.ByteString -> Action ()
historySave k ver store = Action $ do
    Global{..} <- getRO
    Local{localHistory, localProduces, localDepends, localBuiltinVersion} <- getRW
    liftIO $ when localHistory $ whenJust globalHistory $ \history -> do
        -- make sure we throw errors before we get into the history
        evaluate $ rnf k
        evaluate ver
        evaluate store

        let produced = reverse $ map snd localProduces
        deps <- runLocked globalDatabase $ \database ->
            -- technically this could be run without the DB lock, since it reads things that are stable
            forM (reverse localDepends) $ \(Depends is) -> forM is $ \i -> do
                (k, r) <- getIdKeyStatus database i
                let fromReady (Ready r) = r
                return (k, runIdentify globalRules k $ result $ fromReady r)
        addHistory history (newKey k) localBuiltinVersion (Version ver) deps store produced
        liftIO $ globalDiagnostic $ return $ "History saved for " ++ show k


runIdentify :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> BS.ByteString
runIdentify mp k v
    | Just BuiltinRule{..} <- Map.lookup (typeKey k) mp = builtinIdentity k v
    | otherwise = throwImpure $ errorInternal "runIdentify can't find rule"
