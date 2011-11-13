{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Development.Shake.Core where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Binary
import Data.Hashable
import Data.Monoid
import Data.Typeable


---------------------------------------------------------------------
-- OPTIONS

data ShakeOptions = ShakeOptions
    {shakeDatabase :: FilePath -- ^ Where shall I store the database file (defaults to @_database@)
    ,shakeJournal :: FilePath -- ^ Where shall I store the journal file (defaults to @_journal@)
    ,shakeParallelism :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@)
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force everyone to rebuild
    }

shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions "_database" "_journal" 1 0


---------------------------------------------------------------------
-- RULES

class (
    Show key, Typeable key, Eq key, Ord key, Hashable key, Binary key,
    Show value, Typeable value, Eq value, Ord value, Hashable value, Binary value
    ) => Rule key value where
    externalValue :: key -> IO (Maybe value)
    externalValue x = return Nothing


data ARule = forall key value . Rule key value => ARule (key -> Maybe (Make value))

data Rules a = Rules
    {value :: a -- not really used, other than for the Monad instance
    ,actions :: [Make ()]
    ,rules :: [ARule]
    ,defaultRules :: [ARule]
    }

instance Monoid a => Monoid (Rules a) where
    mempty = return mempty
    mappend a b = (a >> b){value = value a `mappend` value b}

instance Monad Rules where
    return x = Rules x [] [] []
    Rules v1 x1 x2 x3 >>= f = Rules v2 (x1++y1) (x2++y2) (x3++y3)
        where Rules v2 y1 y2 y3 = f v1


-- accumulate the Rule instances from defaultRule and rule, and put them in]
-- if no rules to build something then it's cache instance is dodgy anyway
defaultRule :: Rule key value => (key -> Maybe (Make value)) -> Rules ()
defaultRule r = mempty{defaultRules=[ARule r]}


rule :: Rule key value => (key -> Maybe (Make value)) -> Rules ()
rule r = mempty{rules=[ARule r]}


action :: Make a -> Rules ()
action a = mempty{actions=[void a]}


---------------------------------------------------------------------
-- DATABASE

data Database = Database

type Key = ()
type Value = ()


loadDatabase :: ShakeOptions -> Rules () -> IO State
loadDatabase = error "load state"


saveDatabase :: ShakeOptions -> State -> IO ()
saveDatabase opts = error "save state"



---------------------------------------------------------------------
-- MAKE

data S = S
    {database :: Database
    ,stack :: [String]
    ,current :: [[Key]]
    }

newtype Make a = Make {fromMake :: StateT S IO a}
    deriving (Functor, Monad, MonadIO)


apply :: Rule key value => [key] -> Make [value]
apply = mapM apply1

apply1 :: Rule key value => key -> Make value
apply1 key = do
    error "todo: apply1"


run :: ShakeOptions -> Rules () -> IO ()
run opts rules = do
    state <- loadState opts rules
    runReaderT (mapM_ fromMake $ actions rules) state
    saveState opts state
