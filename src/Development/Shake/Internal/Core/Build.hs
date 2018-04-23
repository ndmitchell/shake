{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables, NamedFieldPuns, GADTs #-}
{-# LANGUAGE Rank2Types, ConstraintKinds #-}

module Development.Shake.Internal.Core.Build(
    getDatabaseValue,
    apply, apply1,
    ) where

import Development.Shake.Classes
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Wait
import Development.Shake.Internal.Core.History
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import General.Extra
import qualified General.Intern as Intern
import General.Intern(Id)

import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Numeric.Extra
import Control.Concurrent.Extra
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


type Returns a = forall b . (a -> IO b) -> (Capture a -> IO b) -> IO b


internKey :: InternDB -> StatusDB -> Key -> IO Id
internKey intern status k = do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            writeIORef' intern is
            Ids.insert status i (k,Missing)
            return i


getDatabaseValue :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action (Maybe (Either BS.ByteString value))
getDatabaseValue k = do
    global@Global{globalDatabase=Database{..},..} <- Action getRO
    status <- liftIO $ withLock lock $ do
        i <- internKey intern status $ newKey k
        Ids.lookup status i
    return $ case status of
        Just (_, status) | Just r <- getResult status -> Just $ fromValue <$> result r
        _ -> Nothing


-- | Return either an exception (crash), or (how much time you spent waiting, the value)
build :: Global -> Stack -> [Key] -> Capture (Either SomeException (Seconds,Depends,[Value]))
build global@Global{globalDatabase=Database{..},globalPool=pool,..} stack ks continue =
    join $ withLock lock $ do
        is <- forM ks $ internKey intern status

        buildMany stack is
            (\v -> case v of Error e -> Just e; _ -> Nothing)
            (\v -> return $ continue $ case v of
                Left e -> Left e
                Right rs -> Right (0, Depends is, map result rs)) $
            \go -> do
                -- only bother doing the stack check if we're actually going to build stuff
                whenJust (checkStack is stack) $ \(badId, badKey) ->
                    -- everything else gets thrown via Left and can be Staunch'd
                    -- recursion in the rules is considered a worse error, so fails immediately
                    errorRuleRecursion (showStack stack ++ [show badKey]) (typeKey badKey) (show badKey)

                time <- offsetTime
                go $ \x -> case x of
                    Left e -> addPool PoolException pool $ continue $ Left e
                    Right rs -> addPool PoolResume pool $ do dur <- time; continue $ Right (dur, Depends is, map result rs)
                return $ return ()
    where
        identity = runIdentify globalRules

        (#=) :: Id -> (Key, Status) -> IO Status
        i #= (k,v) = do
            globalDiagnostic $ do
                old <- Ids.lookup status i
                return $ maybe "Missing" (statusType . snd) old ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) old
            Ids.insert status i (k,v)
            return v

        buildMany :: Stack -> [Id] -> (Status -> Maybe a) -> Returns (Either a [Result Value])
        buildMany stack is test fast slow = do
            let toAnswer v | Just v <- test v = Left v
                toAnswer (Ready v) = Right v
            let toCompute (Waiting w _) = Later $ toAnswer <$> w
                toCompute x = Now $ toAnswer x

            res <- waitExcept =<< mapM (fmap toCompute . reduce stack) is
            case res of
                Now v -> fast v
                Later w -> slow $ \slow -> afterWait w slow

        -- Rules for each of the following functions
        -- * Must NOT lock
        -- * Must have an equal return to what is stored in the db at that point
        -- * Must return one of the designated subset of values

        reduce :: Stack -> Id -> IO Status {- Ready | Error | Waiting -}
        reduce stack i = do
            s <- Ids.lookup status i
            case s of
                Nothing -> throwM $ errorInternal $ "interned value missing from database, " ++ show i
                Just (k, Missing) -> spawn RunDependenciesChanged stack i k Nothing
                Just (k, Loaded r) -> check stack i k r (depends r)
                Just (k, res) -> return res


        -- | Given a Key and the list of dependencies yet to be checked, check them
        check :: Stack -> Id -> Key -> Result BS.ByteString -> [Depends] -> IO Status {- Ready | Waiting -}
        check stack i k r [] = spawn RunDependenciesSame stack i k $ Just r
        check stack i k r (Depends ds:rest) = do
            let cont v = if isLeft v then spawn RunDependenciesChanged stack i k $ Just r else check stack i k r rest
            buildMany (addStack i k stack) ds
                (\v -> case v of
                    Error _ -> Just ()
                    Ready dep | changed dep > built r -> Just ()
                    _ -> Nothing)
                cont $
                \go -> do
                    (self, done) <- newWait
                    go $ \v -> do
                        res <- cont v
                        case res of
                            Waiting w _ -> afterWait w done
                            _ -> done res
                    i #= (k, Waiting self $ Just r)


        -- | Given a Key, queue up execution and return waiting
        spawn :: RunMode -> Stack -> Id -> Key -> Maybe (Result BS.ByteString) -> IO Status {- Waiting -}
        spawn mode stack i k r = do
            (w, done) <- newWait
            when (mode == RunDependenciesChanged) $ whenJust history $ \history ->
                whenM (hasHistory history k) $ putStrLn $ "CACHE: Should have checked here, " ++ show k
            addPool PoolStart pool $
                runKey global (addStack i k stack) step k r mode $ \res -> do
                    withLock lock $ do
                        let status = either Error (Ready . runValue . snd) res
                        i #= (k, status)
                        done status
                    case res of
                        Right (produced, RunResult{..}) -> do
                            globalDiagnostic $ return $
                                "result " ++ showBracket k ++ " = "++ showBracket (result runValue) ++
                                " " ++ (if built runValue == changed runValue then "(changed)" else "(unchanged)")
                            unless (runChanged == ChangedNothing) $ do
                                journal i k runValue{result=runStore}
                                unless (runChanged == ChangedStore) $ whenJust history $ \history -> whenJust produced $ \produced -> do
                                    ds <- forM (depends runValue) $ \(Depends is) ->
                                        forM is $ \i -> do
                                            -- if this didn't match then we couldn't have got here
                                            Just (key, Ready value) <- Ids.lookup status i
                                            return (key, identity key $ result value)
                                    addHistory history k ds runStore produced
                        Left _ ->
                            globalDiagnostic $ return $ "result " ++ showBracket k ++ " = error"
            i #= (k, Waiting w r)


-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'addUserRule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: (RuleResult key ~ value, ShakeValue key, Typeable value) => [key] -> Action [value]
-- Don't short-circuit [] as we still want error messages
apply (ks :: [key]) = withResultType $ \(p :: Maybe (Action [value])) -> do
    -- this is the only place a user can inject a key into our world, so check they aren't throwing
    -- in unevaluated bottoms
    liftIO $ mapM_ (evaluate . rnf) ks

    let tk = typeRep (Proxy :: Proxy key)
        tv = typeRep (Proxy :: Proxy value)
    Global{..} <- Action getRO
    Local{localBlockApply} <- Action getRW
    whenJust localBlockApply $ throwM . errorNoApply tk (show <$> listToMaybe ks)
    case Map.lookup tk globalRules of
        Nothing -> throwM $ errorNoRuleToBuildType tk (show <$> listToMaybe ks) (Just tv)
        Just BuiltinRule{builtinResult=tv2} | tv /= tv2 -> throwM $ errorInternal $ "result type does not match, " ++ show tv ++ " vs " ++ show tv2
        _ -> fmap (map fromValue) $ applyKeyValue $ map newKey ks


runIdentify :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> BS.ByteString
runIdentify mp k v
    | Just BuiltinRule{..} <- Map.lookup (typeKey k) mp = builtinIdentity k v
    | otherwise = throwImpure $ errorInternal "runIdentify can't find rule"


applyKeyValue :: [Key] -> Action [Value]
applyKeyValue [] = return []
applyKeyValue ks = do
    global@Global{..} <- Action getRO
    Local{localStack} <- Action getRW
    (dur, dep, vs) <- Action $ captureRAW $ build global localStack ks
    Action $ modifyRW $ \s -> s{localDiscount=localDiscount s + dur, localDepends=dep : localDepends s}
    return vs


runKey
    :: Global
    -> Stack  -- Given the current stack with the key added on
    -> Step  -- And the current step
    -> Key -- The key to build
    -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
    -> RunMode -- True if any of the children were dirty
    -> Capture (Either SomeException (Maybe [FilePath], RunResult (Result Value)))
        -- Either an error, or a (the produced files, the result).
runKey global@Global{globalOptions=ShakeOptions{..},..} stack step k r mode continue = do
    let tk = typeKey k
    BuiltinRule{..} <- case Map.lookup tk globalRules of
        Nothing -> throwM $ errorNoRuleToBuildType tk (Just $ show k) Nothing
        Just r -> return r

    let s = newLocal stack shakeVerbosity
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
                continue . Left . toException =<< shakeException global (showStack stack) e
            Right (RunResult{..}, Local{..})
                | runChanged == ChangedNothing || runChanged == ChangedStore, Just r <- r ->
                    continue $ Right $ (,) produced $ RunResult runChanged runStore (r{result = runValue})
                | otherwise -> do
                    dur <- time
                    let (cr, c) | Just r <- r, runChanged == ChangedRecomputeSame = (ChangedRecomputeSame, changed r)
                                | otherwise = (ChangedRecomputeDiff, step)
                    continue $ Right $ (,) produced $ RunResult cr runStore Result
                        {result = runValue
                        ,changed = c
                        ,built = step
                        ,depends = nubDepends $ reverse localDepends
                        ,execution = doubleToFloat $ dur - localDiscount
                        ,traces = reverse localTraces}
                where produced = if localCache /= CacheYes then Nothing else Just $ reverse $ map snd localProduces


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action value
apply1 = fmap head . apply . return
