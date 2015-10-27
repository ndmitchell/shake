{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_filepath
#if __GLASGOW_HASKELL__ >= 709
#define MIN_VERSION_filepath(a,b,c) 1
#else
#define MIN_VERSION_filepath(a,b,c) 0
#endif
#endif

-- | A module for 'FilePath' operations exposing "System.FilePath" plus some additional operations.
--
--   /Windows note:/ The extension methods ('<.>', 'takeExtension' etc) use the Posix variants since on
--   Windows @\"\/\/\*\" '<.>' \"txt\"@ produces @\"\/\/\*\\\\.txt\"@
--   (which is bad for 'Development.Shake.FilePattern' values).
module Development.Shake.FilePath(
    module System.FilePath, module System.FilePath.Posix,
    dropDirectory1, takeDirectory1, normaliseEx,
#if !MIN_VERSION_filepath(1,4,0)
    (-<.>),
#endif
    toNative, toStandard,
    exe
    ) where

import System.Info.Extra
import qualified System.FilePath as Native

import System.FilePath hiding
    (splitExtension, takeExtension, replaceExtension, dropExtension, addExtension
    ,hasExtension, (<.>), splitExtensions, takeExtensions, dropExtensions
#if MIN_VERSION_filepath(1,4,0)
    ,(-<.>)
#endif
    )
import System.FilePath.Posix
    (splitExtension, takeExtension, replaceExtension, dropExtension, addExtension
    ,hasExtension, (<.>), splitExtensions, takeExtensions, dropExtensions
#if MIN_VERSION_filepath(1,4,0)
    ,(-<.>)
#endif
    )

#if !MIN_VERSION_filepath(1,4,0)
infixr 7  -<.>

-- | Remove the current extension and add another, an alias for 'replaceExtension'.
(-<.>) :: FilePath -> String -> FilePath
(-<.>) = replaceExtension
#endif


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


-- | Normalise a 'FilePath', applying the rules:
--
-- * All 'pathSeparators' become 'pathSeparator' (@\/@ on Linux, @\\@ on Windows)
--
-- * @foo\/bar\/..\/baz@ becomes @foo\/baz@ (not universally true in the presence of symlinks)
--
-- * @foo\/.\/bar@ becomes @foo\/bar@
--
-- * @foo\/\/bar@ becomes @foo\/bar@
--
--   This function is not based on the 'normalise' function from the @filepath@ library, as that function
--   is quite broken.
normaliseEx :: FilePath -> FilePath
normaliseEx xs | a:b:xs <- xs, isWindows && sep a && sep b = '/' : f ('/':xs) -- account for UNC paths being double //
               | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = toNative $ deslash o $ (++"/") $ concatMap ('/':) $ reverse $ g 0 $ reverse $ split o

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
            where (a,b) = break sep ys
                  ys = dropWhile sep xs


-- | Convert to native path separators, namely @\\@ on Windows. 
toNative :: FilePath -> FilePath
toNative = if isWindows then map (\x -> if x == '/' then '\\' else x) else id

-- | Convert all path separators to @/@, even on Windows.
toStandard :: FilePath -> FilePath
toStandard = if isWindows then map (\x -> if x == '\\' then '/' else x) else id


-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: String
exe = if isWindows then "exe" else ""
