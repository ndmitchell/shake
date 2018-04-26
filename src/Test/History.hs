
module Test.History(main) where

import Development.Shake
import Test.Type
import General.GetOpt
import System.Directory
import Data.List
import Control.Monad
import Prelude

data Args = Die deriving (Eq,Enum,Bounded,Show)

main = shakeTest test optionsEnum $ \args ->
    "B.txt" %> \out -> do
        when (Die `elem` args) $ error "Die"
        putLoud "running B.txt"
        copyFile' "A.txt" out


test build = do
    build ["clean"]
    writeFile "A.txt" "1"
    build ["B.txt","--cache","--sleep"]
    assertContents "B.txt" "1"
    writeFile "A.txt" "2"
    build ["B.txt","--cache","--sleep"]
    assertContents "B.txt" "2"

    writeFile "A.txt" "1"
    assertException [] $ build ["B.txt","--die","--quiet"]
    build ["B.txt","--die","--cache","--sleep"]
    assertContents "B.txt" "1"

    writeFile "A.txt" "2"
    removeFile "B.txt"
    build ["B.txt","--die","--cache"]
    assertContents "B.txt" "2"
