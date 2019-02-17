
-- | Working with escape sequences
module General.EscCodes(
    Color(..),
    removeEscCodes,
    escWindowTitle,
    escCursorUp,
    escClearLine,
    escForeground,
    escNormal
    ) where

import Data.Char


removeEscCodes :: String -> String
removeEscCodes ('\ESC':'[':xs) = removeEscCodes $ drop 1 $ dropWhile (not . isAlpha) xs
removeEscCodes (x:xs) = x : removeEscCodes xs
removeEscCodes [] = []


escWindowTitle :: String -> String
escWindowTitle x = "\ESC]0;" ++ x ++ "\BEL"

escCursorUp :: Int -> String
escCursorUp i = "\ESC[" ++ show i ++ "A"

escClearLine :: String
escClearLine = "\ESC[K"


data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Show,Enum)

escForeground :: Color -> String
escForeground x = "\ESC[" ++ show (30 + fromEnum x) ++ "m"

escNormal :: String
escNormal = "\ESC[0m"
