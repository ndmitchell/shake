
-- | Module for 'FilePath' operations, to be used in place of
--   "System.FilePath". It uses @\/@ as the directory separator to ensure
--   uniqueness, and squashes any @\/.\/@ components.
module Development.Shake.FilePath(
    module System.FilePath.Posix,
    dropDirectory1, takeDirectory1, normalise,
    toNative, (</>), combine,
    ) where

import System.FilePath.Posix hiding (normalise, (</>), combine)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath as Native


-- | Drop the first directory from a 'FilePath'. Should only be used on
--   relative paths.
--
-- > dropDirectory1 "aaa/bbb" == "bbb"
-- > dropDirectory1 "aaa/" == ""
-- > dropDirectory1 "aaa" == ""
-- > dropDirectory1 "" == ""
dropDirectory1 :: FilePath -> FilePath
dropDirectory1 = drop 1 . dropWhile (not . isPathSeparator)


-- | Take the first component of a 'FilePath'. Should only be used on
--   relative paths.
--
-- > takeDirectory1 "aaa/bbb" == "aaa"
-- > takeDirectory1 "aaa/" == "aaa"
-- > takeDirectory1 "aaa" == "aaa"
takeDirectory1 :: FilePath -> FilePath
takeDirectory1 = takeWhile (not . isPathSeparator)


normalise :: FilePath -> FilePath
normalise = map (\x -> if x == '\\' then '/' else x)


toNative :: FilePath -> FilePath
toNative = map (\x -> if x == '/' then Native.pathSeparator else x)


(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine


combine :: FilePath -> FilePath -> FilePath
combine x ('.':'.':'/':y) = combine (takeDirectory x) y
combine x y = Posix.combine x y
