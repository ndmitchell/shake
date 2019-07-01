{-# LANGUAGE TypeFamilies #-}

module Test.History(main) where

import Control.Monad
import Development.Shake
import Test.Type
import General.Extra
import General.GetOpt
import System.Directory


data Args = Die deriving (Eq,Enum,Bounded,Show)

type instance RuleResult FilePath = String

main = testBuildArgs test optionsEnum $ \args -> do
    let die :: a -> a
        die x = if Die `elem` args then error "Die" else x

    phony "Phony" $ return ()
    "Phony.txt" %> \out -> do
        need ["Phony"]
        copyFile' "In.txt" out

    "OutFile.txt" %> \out -> die $ copyFile' "In.txt" out

    reader <- addOracleCache $ \x -> die (readFile' x)
    "OutOracle.txt" %> \out -> do
        historyDisable
        writeFile' out =<< reader "In.txt"

    ["OutFiles1.txt","OutFiles2.txt"] &%> \[out1, out2] -> die $ do
        copyFile' "In.txt" out1
        copyFile' "In.txt" out2

test build =
    forM_ [[],["--share-copy"]] $ \args -> do
        let setIn = writeFile "In.txt"
        let outs = ["OutFile.txt","OutOracle.txt","OutFiles1.txt","OutFiles2.txt","Phony.txt"]
        let checkOut x = mapM_ (`assertContents` x) outs

        build ["clean"]
        setIn "1"
        build $ args ++ ["--share","--sleep"] ++ outs
        checkOut "1"
        setIn "2"
        build $ args ++ ["--share","--sleep"] ++ outs
        checkOut "2"

        setIn "1"
        assertException [] $ build ["OutFile.txt","--die","--quiet","--sleep"]
        build $ args ++ ["--die","--share"] ++ outs
        checkOut "1"

        setIn "2"
        mapM_ removeFile_ outs
        build $ args ++ ["--die","--share"] ++ outs
        checkOut "2"

        setIn "2"
        removeFile ".shake.database"
        build $ args ++ ["--die","--share"] ++ outs
        checkOut "2"
