
-- | A module for 'FilePath' operations, to be used instead of "System.FilePath"
--   when writing build systems. In build systems, when using the file name
--   as a key for indexing rules, it is important that two different strings do
--   not refer to the same on-disk file. We therefore follow the conventions:
--
-- * Always use @\/@ as the directory separator, even on Windows.
--
-- * When combining 'FilePath' values with '</>' we squash any @\/.\/@ components.
module Development.Shake.FilePath(
    module System.FilePath.Posix, -- apart from what I override and search path stuff
    module System.FilePath, -- only search-path stuff
    dropDirectory1, takeDirectory1, normalise,
    (-<.>),
    toNative, toStandard, (</>), combine,
    exe
    ) where

import System.FilePath.Posix hiding
    (normalise, (</>), combine
    ,searchPathSeparator, isSearchPathSeparator, splitSearchPath, getSearchPath)
import System.FilePath(searchPathSeparator, isSearchPathSeparator, splitSearchPath, getSearchPath)
import General.Base
import qualified System.FilePath as Native

infixr 5  </>
infixr 7  -<.>


-- | Drop the first directory from a 'FilePath'. Should only be used on
--   relative paths.
--
-- > dropDirectory1 "aaa/bbb" == "bbb"
-- > dropDirectory1 "aaa/" == ""
-- > dropDirectory1 "aaa" == ""
-- > dropDirectory1 "" == ""
dropDirectory1 :: FilePath -> FilePath
dropDirectory1 = drop 1 . dropWhile (not . Native.isPathSeparator)


-- | Take the first component of a 'FilePath'. Should only be used on
--   relative paths.
--
-- > takeDirectory1 "aaa/bbb" == "aaa"
-- > takeDirectory1 "aaa/" == "aaa"
-- > takeDirectory1 "aaa" == "aaa"
takeDirectory1 :: FilePath -> FilePath
takeDirectory1 = takeWhile (not . Native.isPathSeparator)


-- | Normalise a 'FilePath', trying to do:
--
-- * All 'pathSeparators' become @\/@
--
-- * @foo\/bar\/..\/baz@ becomes @foo\/baz@
--
-- * @foo\/.\/bar@ becomes @foo\/bar@
--
-- * @foo\/\/bar@ becomes @foo\/bar@
--
--   This function is not based on the normalise function from the filepath library, as that function
--   is quite broken.
normalise :: FilePath -> FilePath
normalise xs | a:b:xs <- xs, isWindows && sep a && sep b = '/' : f ('/':xs) -- account for UNC paths being double //
             | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = deslash o $ (++"/") $ concatMap ('/':) $ reverse $ g 0 $ reverse $ split o

        deslash o x
            | x == "/" = case (pre,pos) of
                (True,True) -> "/"
                (True,False) -> "/."
                (False,True) -> "./"
                (False,False) -> "."
            | otherwise = (if pre then id else tail) $ (if pos then id else init) x
            where pre = sep $ head $ o ++ " "
                  pos = sep $ last $ " " ++ o

        g i [] = replicate i ".."
        g i ("..":xs) = g (i+1) xs
        g i (".":xs) = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (x:xs) = g (i-1) xs

        split xs = if null ys then [] else a : split b
            where (a,b) = break sep $ ys
                  ys = dropWhile sep xs


-- | Convert to native path separators, namely @\\@ on Windows. 
toNative :: FilePath -> FilePath
toNative = map (\x -> if Native.isPathSeparator x then Native.pathSeparator else x)

-- | Convert all path separators to @/@, even on Windows.
toStandard :: FilePath -> FilePath
toStandard | Native.pathSeparators == [pathSeparator] = id
           | otherwise = map (\x -> if Native.isPathSeparator x then pathSeparator else x)


-- | Combine two file paths, an alias for 'combine'.
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

-- | Remove the current extension and add another, an alias for 'replaceExtension'.
(-<.>) :: FilePath -> String -> FilePath
(-<.>) = replaceExtension

-- | Combine two file paths. Any redundant @.\/@ or @..\/@ components in the
--   resulting path are eliminated - the result will always have 'normalise' applied.
--
-- > combine "aaa/bbb" "ccc" == "aaa/bbb/ccc"
-- > combine "aaa/bbb" "./ccc" == "aaa/bbb/ccc"
-- > combine "aaa/bbb" "../ccc" == "aaa/ccc"
combine :: FilePath -> FilePath -> FilePath
combine x y = normalise $ Native.combine (toNative x) (toNative y)


-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: String
exe = if isWindows then "exe" else ""
