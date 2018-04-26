{-# LANGUAGE TypeFamilies #-}

module Test.History(main) where

import Development.Shake
import Test.Type
import General.GetOpt
import System.Directory
import Data.List
import Control.Monad
import Prelude

data Args = Die deriving (Eq,Enum,Bounded,Show)

type instance RuleResult FilePath = String

main = shakeTest test optionsEnum $ \args -> do
    let die :: a -> a
        die x = if Die `elem` args then error "Die" else x

    "OutFile.txt" %> \out -> die $ copyFile' "In.txt" out

    reader <- addOracleCache $ \x -> die (readFile' x)
    "OutOracle.txt" %> \out -> do
        historyDisable
        writeFile' out =<< reader "In.txt"

test build = do
    let setIn = writeFile "In.txt"
    let outs = ["OutFile.txt","OutOracle.txt"]
    let checkOut x = mapM_ (`assertContents` x) outs

    build ["clean"]
    setIn "1"
    build $ ["--cache","--sleep"] ++ outs
    checkOut "1"
    setIn "2"
    build $ ["--cache","--sleep"] ++ outs
    checkOut "2"

    setIn "1"
    assertException [] $ build ["OutFile.txt","--die","--quiet","--sleep"]
    build $ ["--die","--cache"] ++ outs
    checkOut "1"

    setIn "2"
    mapM_ removeFile outs
    build $ ["--die","--cache"] ++ outs
    checkOut "2"
