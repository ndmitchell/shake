
module Development.Shake.ByteString(parseMakefile, normalise, linesCR) where

import qualified Data.ByteString.Char8 as BS
import qualified System.FilePath as Native
import Development.Shake.Prelude


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
        f o = BS.pack $ deslash o $ (++"/") $ concatMap ('/':) $ reverse $ g 0 $ reverse $ split $ BS.unpack o

        deslash o x
            | x == "/" = case (pre,pos) of
                (True,True) -> "/"
                (True,False) -> "/."
                (False,True) -> "./"
                (False,False) -> "."
            | otherwise = (if pre then id else tail) $ (if pos then id else init) x
            where pre = sep $ head $ BS.unpack o ++ " "
                  pos = sep $ last $ " " ++ BS.unpack o

        g i [] = replicate i ".."
        g i ("..":xs) = g (i+1) xs
        g i (".":xs) = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (x:xs) = g (i-1) xs

        split xs = if null ys then [] else a : split b
            where (a,b) = break sep $ ys
                  ys = dropWhile sep xs
