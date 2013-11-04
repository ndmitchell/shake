
module Development.Shake.ByteString(parseMakefile, normalise, linesCR) where

import qualified Data.ByteString.Char8 as BS
import qualified System.FilePath as Native
import Development.Shake.Prelude
import Data.List


endsSlash :: BS.ByteString -> Bool
endsSlash = BS.isSuffixOf (BS.singleton '\\')


parseMakefile :: BS.ByteString -> [(BS.ByteString, [BS.ByteString])]
parseMakefile = concatMap f . join . linesCR
    where
        join xs = case span endsSlash xs of
            ([], []) -> []
            (xs, []) -> [BS.unwords $ map BS.init xs]
            ([], y:ys) -> y : join ys
            (xs, y:ys) -> BS.unwords (map BS.init xs ++ [y]) : join ys

        f x = [(a, BS.words $ BS.drop 1 b) | a <- BS.words a]
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


normalise :: BS.ByteString -> BS.ByteString
normalise xs | isWindows, Just (a,xs) <- BS.uncons xs, sep a, Just (b,_) <- BS.uncons xs, sep b = '/' `BS.cons` f xs
             | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f = BS.pack . reverse . redot . dropDot . intercalate "/" . dropDots . resep . reverse . BS.unpack

        dropDot ('/':'.':'/':c) = dropDot $ '/' : c
        dropDot (x:xs) = x : dropDot xs
        dropDot [] = []

        redot x | x == "" = "."
                | Just x@(_:_) <- stripPrefix "./" x = redot x
                | Just x@(_:_) <- stripPrefix "./" $ reverse x = redot $ reverse x
                | otherwise = x

        dropDots x = h x
            where
                h xs = f 0 $ break (== '/') xs
            
                g i "" = replicate i ".."
                g i ('/':xs) = f i $ break (== '/') xs
            
                f i ("..",xs) = g (i+1) xs
                f i (".",xs) = "." : g i xs
                f 0 (x,xs) = x : g 0 xs
                f i ("",[]) = replicate i ".." ++ [""]
                f i (x,xs) = "." : g (i-1) xs

        resep (x:xs) | sep x = '/' : resep (dropWhile sep xs)
        resep (x:xs) = x : resep xs
        resep [] = []
