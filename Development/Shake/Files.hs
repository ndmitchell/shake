
module Development.Shake.Files() where

import Control.Monad


newtype File = File FilePath



(*>) :: FilePath -> (FilePath -> Make ()) -> Rules ()
(*>) pat f = rule1 $ \x -> if not $ x ?= pat then Nothing else Just $
    \x -> do f x; getModificationTime x


need :: [FilePath] -> Make ()
need xs = void $ run $ map File xs



want :: [FilePath] -> Rule ()
want xs = action $ need $ map File xs
