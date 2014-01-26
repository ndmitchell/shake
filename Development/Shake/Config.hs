{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | A module for parsing and using config files in a Shake build system. Config files
--   consist of variable bindings, for example:
--
-- > # This is my Config file
-- > HEADERS_DIR = /path/to/dir
-- > CFLAGS = -g -I${HEADERS_DIR}
-- > CFLAGS = $CFLAGS -O2
-- > include extra/file.cfg
--
--   This defines the variable @HEADERS_DIR@ (equal to @\/path\/to\/dir@), and
--   @CFLAGS@ (equal to @-g -I\/path\/to\/dir -O2@), and also includes the configuration
--   statements in the file @extra/file.cfg@. The full lexical syntax for configuration
--   files is defined here: <http://martine.github.io/ninja/manual.html#_lexical_syntax>.
--
--   To use the configuration file either use 'readConfigFile' to parse the configuration file
--   and use the values directly, or 'usingConfigFile' and 'getConfig' to track the configuration
--   values, so they become build dependencies.
module Development.Shake.Config(
    readConfigFile,
    usingConfigFile, usingConfig,
    getConfig
    ) where

import Development.Shake
import Development.Shake.Classes
import qualified Development.Ninja.Parse as Ninja
import qualified Development.Ninja.Env as Ninja
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.UTF8 as UTF8
import Control.Applicative
import Control.Arrow


-- | Read a config file, returning a list of the variables and their bindings.
--   Config files use the Ninja lexical syntax:
--   <http://martine.github.io/ninja/manual.html#_lexical_syntax>
readConfigFile :: FilePath -> IO (Map.HashMap String String)
readConfigFile file = do
    env <- Ninja.newEnv
    Ninja.parse file env
    mp <- Ninja.fromEnv env
    return $ Map.fromList $ map (UTF8.toString *** UTF8.toString) $ Map.toList mp


newtype Config = Config String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)


-- | Specify the file to use with 'getConfig'.
usingConfigFile :: FilePath -> Rules ()
usingConfigFile file = do
    mp <- newCache $ \() -> liftIO $ readConfigFile file
    addOracle $ \(Config x) -> Map.lookup x <$> mp ()
    return ()


-- | Specify the values to use with 'getConfig', generally prefer
--   'usingConfigFile' unless you also need access to the values
--   of variables outside 'Action'.
usingConfig :: Map.HashMap String String -> Rules ()
usingConfig mp = do
    addOracle $ \(Config x) -> return $ Map.lookup x mp
    return ()


-- | Obtain the value of a configuration variable, returns 'Nothing' to indicate the variable
--   has no binding. Any build system using 'getConfig' /must/ call either 'usingConfigFile'
--   or 'usingConfig'. The 'getConfig' function will introduce a dependency on the configuration
--   variable (but not the whole configuration file), and if the configuration variable changes, the rule will be rerun.
getConfig :: String -> Action (Maybe String)
getConfig = askOracle . Config
