
module Start(main) where

import Development.Make.All
import System.Environment
import Development.Shake
import qualified System.Directory as IO
import System.Console.GetOpt


main :: IO ()
main = do
    args <- getArgs
    withArgs ("--no-time":args) $
        shakeArgsWith shakeOptions flags $ \opts targets -> do
            makefile <- case reverse [x | UseMakefile x <- opts] of
                x:_ -> return x
                _ -> findMakefile
            fmap Just $ runMakefile makefile targets


data Flag = UseMakefile FilePath

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."]


findMakefile :: IO FilePath
findMakefile = do
    b <- IO.doesFileExist "makefile"
    if b then return "makefile" else do
        b <- IO.doesFileExist "Makefile"
        if b then return "Makefile" else
            error "Could not find either `makefile' or `Makefile'"
