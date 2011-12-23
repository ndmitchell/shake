{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Test.Errors(main) where

import Development.Shake
import Examples.Util
import Control.Exception hiding (assert)
import Control.Monad
import Data.List


main = shaken test $ \args obj -> do
    want $ map obj args

    obj "norule" *> \_ ->
        need [obj "norule_isavailable"]

    obj "failcreate" *> \_ ->
        return ()

    obj "recursive" *> \out ->
        need [out]

    obj "systemcmd" *> \_ ->
        system' "random_missing_command" []

    obj "stack1" *> \_ -> need [obj "stack2"]
    obj "stack2" *> \_ -> need [obj "stack3"]
    obj "stack3" *> \_ -> error "crash"


test build obj = do
    let crash args parts = do
            res <- try $ build args
            case res of
                Left (err :: SomeException) -> let s = show err in forM_ parts $ \p ->
                    assert (p `isInfixOf` s) $ "Incorrect exception, missing part:\nGOT: " ++ s ++ "\nWANTED: " ++ p
                Right _ -> error "Expected an exception but succeeded"

    crash ["norule"] ["norule_isavailable"]
    crash ["failcreate"] ["failcreate"]
    crash ["recursive"] ["recursive"]
    crash ["systemcmd"] ["systemcmd","random_missing_command"]
    crash ["stack1"] ["stack1","stack2","stack3","crash"]
