
module Development.Shake(
    module Development.Shake.Core,
    module Development.Shake.Derived
    ) where

import Development.Shake.Core
import Development.Shake.Derived
import Development.Shake.Ls


shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = undefined $ r >> lsRules



need :: [FilePath] -> Make ()
need xs = void $ run $ map File xs

want :: [FilePath] -> Rule ()
want xs = ruleAlways $ need xs

(*>) :: WildFilePath -> (FilePath -> Make ()) -> Rule ()
(*>) x f = rule1 $ \(File s) ->
    if s ?= x then Just $ do f s; Stamp $ getModification s else Nothing
