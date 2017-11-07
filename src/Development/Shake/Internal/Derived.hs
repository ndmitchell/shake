{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shake.Internal.Derived(
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged,
    withTempFile, withTempDir,
    getHashedShakeVersion,
    getShakeExtra, getShakeExtraRules, addShakeExtra,
    par, forP,
    newResource, newThrottle, withResources,
    newCache
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import System.Directory
import System.FilePath (takeDirectory)
import System.IO.Extra hiding (withTempFile, withTempDir, readFile')

import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Options
import Development.Shake.Internal.Rules.File
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import General.Extra
import Data.List.Extra
import Data.Hashable
import Data.Typeable.Extra
import Data.Dynamic
import Prelude


-- | Get a checksum of a list of files, suitable for using as `shakeVersion`.
--   This will trigger a rebuild when the Shake rules defined in any of the files are changed.
--   For example:
--
-- @
-- main = do
--     ver <- 'getHashedShakeVersion' [\"Shakefile.hs\"]
--     'shakeArgs' 'shakeOptions'{'shakeVersion' = ver} ...
-- @
--
--   To automatically detect the name of the current file, turn on the @TemplateHaskell@
--   extension and write @$(LitE . StringL . loc_filename \<$\> location)@.
--
--   This feature can be turned off during development by passing
--   the flag @--no-rule-version@ or setting 'shakeVersionIgnore' to 'True'.
getHashedShakeVersion :: [FilePath] -> IO String
getHashedShakeVersion files = do
    hashes <- mapM (fmap (hashWithSalt 0) . BS.readFile) files
    return $ "hash-" ++ show (hashWithSalt 0 hashes)


-- | Get an item from 'shakeExtra', using the requested type as the key. Fails
-- if the value found at this key does not match the requested type.
getShakeExtra :: Typeable a => Action (Maybe a)
getShakeExtra = liftIO . lookupShakeExtra . shakeExtra =<< getShakeOptions

-- | A version of 'getShakeExtra' in 'Rules'.
getShakeExtraRules :: Typeable a => Rules (Maybe a)
getShakeExtraRules = liftIO . lookupShakeExtra . shakeExtra =<< getShakeOptionsRules

lookupShakeExtra :: forall a . Typeable a => Map.HashMap TypeRep Dynamic -> IO (Maybe a)
lookupShakeExtra mp =
    case Map.lookup want mp of
        Just dyn
            | Just x <- fromDynamic dyn -> return $ Just x
            | otherwise -> errorStructured
                "shakeExtra value is malformed, all keys and values must agree"
                [("Key", Just $ show want)
                ,("Value", Just $ show $ dynTypeRep dyn)]
                "Use addShakeExtra to ensure shakeExtra is well-formed"
        Nothing -> return Nothing
    where want = typeRep (Proxy :: Proxy a)

-- | Add a properly structued value to 'shakeExtra' which can be retrieved with 'getShakeExtra'.
addShakeExtra :: Typeable a => a -> Map.HashMap TypeRep Dynamic -> Map.HashMap TypeRep Dynamic
addShakeExtra x = Map.insert (typeOf x) (toDyn x)


-- | @copyFile' old new@ copies the existing file from @old@ to @new@.
--   The @old@ file will be tracked as a dependency.
--   Also creates the new directory if necessary.
copyFile' :: FilePath -> FilePath -> Action ()
copyFile' old new = do
    need [old]
    putLoud $ "Copying from " ++ old ++ " to " ++ new
    liftIO $ do
        createDirectoryRecursive $ takeDirectory new
        copyFile old new

-- | @copyFileChanged old new@ copies the existing file from @old@ to @new@, if the contents have changed.
--   The @old@ file will be tracked as a dependency.
--   Also creates the new directory if necessary.
copyFileChanged :: FilePath -> FilePath -> Action ()
copyFileChanged old new = do
    need [old]
    -- in newer versions of the directory package we can use copyFileWithMetadata which (we think) updates
    -- the timestamp as well and thus no need to read the source file twice.
    unlessM (liftIO $ doesFileExist new &&^ fileEq old new) $ do
        putLoud $ "Copying from " ++ old ++ " to " ++ new
        liftIO $ do
            createDirectoryRecursive $ takeDirectory new
            -- copyFile does a lot of clever stuff with permissions etc, so make sure we just reuse it
            liftIO $ copyFile old new


-- | Read a file, after calling 'need'. The argument file will be tracked as a dependency.
readFile' :: FilePath -> Action String
readFile' x = need [x] >> liftIO (readFile x)

-- | Write a file, lifted to the 'Action' monad.
writeFile' :: MonadIO m => FilePath -> String -> m ()
writeFile' name x = liftIO $ writeFile name x


-- | A version of 'readFile'' which also splits the result into lines.
--   The argument file will be tracked as a dependency.
readFileLines :: FilePath -> Action [String]
readFileLines = fmap lines . readFile'

-- | A version of 'writeFile'' which writes out a list of lines.
writeFileLines :: MonadIO m => FilePath -> [String] -> m ()
writeFileLines name = writeFile' name . unlines


-- | Write a file, but only if the contents would change.
writeFileChanged :: MonadIO m => FilePath -> String -> m ()
writeFileChanged name x = liftIO $ do
    b <- doesFileExist name
    if not b then writeFile name x else do
        -- Cannot use ByteString here, since it has different line handling
        -- semantics on Windows
        b <- withFile name ReadMode $ \h -> do
            src <- hGetContents h
            return $! src /= x
        when b $ writeFile name x


-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes (provided the file is not still open).
--   The 'FilePath' will not have any file extension, will exist, and will be zero bytes long.
--   If you require a file with a specific name, use 'withTempDir'.
withTempFile :: (FilePath -> Action a) -> Action a
withTempFile act = do
    (file, del) <- liftIO newTempFile
    act file `actionFinally` del


-- | Create a temporary directory inside the system temporary directory.
--   The directory will be deleted after the action completes. As an example:
--
-- @
-- 'withTempDir' $ \\mydir -> do
--    'putNormal' $ \"Temp directory is \" ++ mydir
--    'writeFile'' (mydir \</\> \"test.txt\") \"writing out a temp file\"
-- @
withTempDir :: (FilePath -> Action a) -> Action a
withTempDir act = do
    (dir,del) <- liftIO newTempDir
    act dir `actionFinally` del


-- | A 'parallel' version of 'forM'.
forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = parallel $ map f xs

-- | Execute two operations in parallel, based on 'parallel'.
par :: Action a -> Action b -> Action (a,b)
par a b = do [Left a, Right b] <- parallel [Left <$> a, Right <$> b]; return (a,b)


-- | Create a finite resource, given a name (for error messages) and a quantity of the resource that exists.
--   Shake will ensure that actions using the same finite resource do not execute in parallel.
--   As an example, only one set of calls to the Excel API can occur at one time, therefore
--   Excel is a finite resource of quantity 1. You can write:
--
-- @
-- 'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=2} $ do
--    'Development.Shake.want' [\"a.xls\",\"b.xls\"]
--    excel <- 'Development.Shake.newResource' \"Excel\" 1
--    \"*.xls\" 'Development.Shake.%>' \\out ->
--        'Development.Shake.withResource' excel 1 $
--            'Development.Shake.cmd' \"excel\" out ...
-- @
--
--   Now the two calls to @excel@ will not happen in parallel.
--
--   As another example, calls to compilers are usually CPU bound but calls to linkers are usually
--   disk bound. Running 8 linkers will often cause an 8 CPU system to grid to a halt. We can limit
--   ourselves to 4 linkers with:
--
-- @
-- disk <- 'Development.Shake.newResource' \"Disk\" 4
-- 'Development.Shake.want' [show i 'Development.Shake.FilePath.<.>' \"exe\" | i <- [1..100]]
-- \"*.exe\" 'Development.Shake.%>' \\out ->
--     'Development.Shake.withResource' disk 1 $
--         'Development.Shake.cmd' \"ld -o\" [out] ...
-- \"*.o\" 'Development.Shake.%>' \\out ->
--     'Development.Shake.cmd' \"cl -o\" [out] ...
-- @
newResource :: String -> Int -> Rules Resource
newResource name mx = liftIO $ newResourceIO name mx


-- | Create a throttled resource, given a name (for error messages) and a number of resources (the 'Int') that can be
--   used per time period (the 'Double' in seconds). Shake will ensure that actions using the same throttled resource
--   do not exceed the limits. As an example, let us assume that making more than 1 request every 5 seconds to
--   Google results in our client being blacklisted, we can write:
--
-- @
-- google <- 'Development.Shake.newThrottle' \"Google\" 1 5
-- \"*.url\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.withResource' google 1 $
--         'Development.Shake.cmd' \"wget\" [\"http:\/\/google.com?q=\" ++ 'Development.Shake.FilePath.takeBaseName' out] \"-O\" [out]
-- @
--
--   Now we will wait at least 5 seconds after querying Google before performing another query. If Google change the rules to
--   allow 12 requests per minute we can instead use @'Development.Shake.newThrottle' \"Google\" 12 60@, which would allow
--   greater parallelisation, and avoid throttling entirely if only a small number of requests are necessary.
--
--   In the original example we never make a fresh request until 5 seconds after the previous request has /completed/. If we instead
--   want to throttle requests since the previous request /started/ we can write:
--
-- @
-- google <- 'Development.Shake.newThrottle' \"Google\" 1 5
-- \"*.url\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.withResource' google 1 $ return ()
--     'Development.Shake.cmd' \"wget\" [\"http:\/\/google.com?q=\" ++ 'Development.Shake.FilePath.takeBaseName' out] \"-O\" [out]
-- @
--
--   However, the rule may not continue running immediately after 'Development.Shake.withResource' completes, so while
--   we will never exceed an average of 1 request every 5 seconds, we may end up running an unbounded number of
--   requests simultaneously. If this limitation causes a problem in practice it can be fixed.
newThrottle :: String -> Int -> Double -> Rules Resource
newThrottle name count period = liftIO $ newThrottleIO name count period


-- | Run an action which uses part of several finite resources. Acquires the resources in a stable
--   order, to prevent deadlock. If all rules requiring more than one resource acquire those
--   resources with a single call to 'withResources', resources will not deadlock.
withResources :: [(Resource, Int)] -> Action a -> Action a
withResources res act
    | (r,i):_ <- filter ((< 0) . snd) res = error $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show i
    | otherwise = f $ groupSort res
    where
        f [] = act
        f ((r,xs):rs) = withResource r (sum xs) $ f rs


-- | Given an action on a key, produce a cached version that will execute the action at most once per key.
--   Using the cached result will still result include any dependencies that the action requires.
--   Each call to 'newCache' creates a separate cache that is independent of all other calls to 'newCache'.
--
--   This function is useful when creating files that store intermediate values,
--   to avoid the overhead of repeatedly reading from disk, particularly if the file requires expensive parsing.
--   As an example:
--
-- @
-- digits \<- 'newCache' $ \\file -> do
--     src \<- readFile\' file
--     return $ length $ filter isDigit src
-- \"*.digits\" 'Development.Shake.%>' \\x -> do
--     v1 \<- digits ('dropExtension' x)
--     v2 \<- digits ('dropExtension' x)
--     'Development.Shake.writeFile'' x $ show (v1,v2)
-- @
--
--   To create the result @MyFile.txt.digits@ the file @MyFile.txt@ will be read and counted, but only at most
--   once per execution.
newCache :: (Eq k, Hashable k) => (k -> Action v) -> Rules (k -> Action v)
newCache = liftIO . newCacheIO
