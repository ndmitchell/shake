
module Development.Shake.Derived() where

import Control.Monad


rule1 :: (from -> Maybe (Make to)) -> Rules ()



rulesUncached = rule1 $ \Uncached -> Just $ return now
