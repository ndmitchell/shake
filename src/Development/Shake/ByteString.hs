
module Development.Shake.ByteString(parseMakefile, filepathNormalise, linesCR) where

import qualified Data.ByteString.Char8 as BS
import qualified System.FilePath as Native
import System.Info.Extra
import Data.Char
import Data.List


endsSlash :: BS.ByteString -> Bool
endsSlash = BS.isSuffixOf (BS.singleton '\\')

wordsMakefile :: BS.ByteString -> [BS.ByteString]
wordsMakefile = f . BS.splitWith isSpace
    where
        f (x:xs) | BS.null x = f xs
        f (x:y:xs) | endsSlash x = BS.concat [BS.init x, BS.singleton ' ', y] : f xs
        f (x:xs) = x : f xs
        f [] = []

parseMakefile :: BS.ByteString -> [(BS.ByteString, [BS.ByteString])]
parseMakefile = concatMap f . join . linesCR
    where
        join xs = case span endsSlash xs of
            ([], []) -> []
            (xs, []) -> [BS.unwords $ map BS.init xs]
            ([], y:ys) -> y : join ys
            (xs, y:ys) -> BS.unwords (map BS.init xs ++ [y]) : join ys

        f x = [(a, wordsMakefile $ BS.drop 1 b) | a <- wordsMakefile a]
            where (a,b) = BS.break (== ':') $ BS.takeWhile (/= '#') x


-- | This is a hot-spot, so optimised
linesCR :: BS.ByteString -> [BS.ByteString]
linesCR x = case BS.split '\n' x of
    x:xs | Just ('\r',x) <- unsnoc x -> x : map (\x -> case unsnoc x of Just ('\r',x) -> x; _ -> x) xs
    xs -> xs
    where
        -- the ByteString unsnoc was introduced in a newer version
        unsnoc x | BS.null x = Nothing
                 | otherwise = Just (BS.last x, BS.init x)


filepathNormalise :: BS.ByteString -> BS.ByteString
filepathNormalise xs
    | isWindows, Just (a,xs) <- BS.uncons xs, sep a, Just (b,_) <- BS.uncons xs, sep b = '/' `BS.cons` f xs
    | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = deslash o $ BS.concat $ (slash:) $ intersperse slash $ reverse $ (BS.empty:) $ g 0 $ reverse $ split $ o

        deslash o x
            | x == slash = case (pre,pos) of
                (True,True) -> slash
                (True,False) -> BS.pack "/."
                (False,True) -> BS.pack "./"
                (False,False) -> dot
            | otherwise = (if pre then id else BS.tail) $ (if pos then id else BS.init) x
            where pre = not (BS.null o) && sep (BS.head o)
                  pos = not (BS.null o) && sep (BS.last o)

        g i [] = replicate i dotDot
        g i (x:xs) | BS.null x = g i xs
        g i (x:xs) | x == dotDot = g (i+1) xs
        g i (x:xs) | x == dot = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (x:xs) = g (i-1) xs

        split xs = BS.splitWith sep xs

dotDot = BS.pack ".."
dot = BS.singleton '.'
slash = BS.singleton '/'
