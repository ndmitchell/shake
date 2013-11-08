
module Development.Shake.Util(
    parseMakefile, needMakefileDependencies
    ) where

import Development.Shake
import Development.Shake.File
import qualified Data.ByteString.Char8 as BS
import qualified Development.Shake.ByteString as BS
import Control.Arrow


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = map (BS.unpack *** map BS.unpack) . BS.parseMakefile . BS.pack


needMakefileDependencies :: FilePath -> Action ()
needMakefileDependencies file = needBS . concatMap snd . BS.parseMakefile =<< liftIO (BS.readFile file)
