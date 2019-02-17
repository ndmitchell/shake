
-- | Working with escape sequences
module General.EscCodes(
    removeEscCodes,
    ) where

import Data.Char

removeEscCodes :: String -> String
removeEscCodes ('\ESC':'[':xs) = removeEscCodes $ drop 1 $ dropWhile (not . isAlpha) xs
removeEscCodes (x:xs) = x : removeEscCodes xs
removeEscCodes [] = []
