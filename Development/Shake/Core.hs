
module Development.Shake.Core(Rules, Make, ruleAlways, rules, run, runRules) where

import Control.Monad


data Rules a = Rules
    {actions :: [Make ()]
    ,rules :: TypeMap (from -> Maybe ([from], Make [to]))
    }
    deriving (Monoid, Monad)


data Make a = everything as before
    deriving (Monad, LiftIO)


action :: Make a -> Rules ()
action = error "add to actions"


rule :: (from -> Maybe ([from], Make [to])) -> Rules ()
rule = error "add to rules"


apply :: [from] -> Make [to]
apply xs = error "just invoke the apply"


run :: RunOpts -> Rules () -> IO ()
run = error "start running"
