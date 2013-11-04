
module Development.Shake.ByteString(parseMakefile, linesCR) where

import qualified Data.ByteString.Char8 as BS


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

