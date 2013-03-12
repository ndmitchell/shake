{-# LANGUAGE RecordWildCards #-}

module Development.Make.Run(findMakefile, runMakefile) where

import Development.Shake
import Development.Make.Parse
import Development.Make.Type
import System.Directory as IO
import Data.List
import Data.Maybe
import Control.Monad
import System.Cmd
import System.Exit


findMakefile :: IO FilePath
findMakefile = do
    b <- IO.doesFileExist "makefile"
    if b then return "makefile" else do
        b <- IO.doesFileExist "Makefile"
        if b then return "Makefile" else
            error "Could not find either `makefile' or `Makefile'"


runMakefile :: FilePath -> IO (Rules ())
runMakefile file = do
    make <- parse file
    Makefile rs <- return $ eval make
    print $ rs
    return $ convert rs


eval :: Makefile -> Makefile
eval (Makefile xs) = Makefile [Rule (f a) (f b) (map f c) | Rule a b c <- xs]
    where f = substitute [(a,b) | Variable a b <- xs]


convert :: [Stmt] -> Rules ()
convert rs = match ?> run
    where
        check s r = msum $ map (flip makePattern s) $ words $ flatten $ targets r

        run s = need (concat deps) >> mapM_ runCommand (concat cmds)
            where
                flat = flatten . substitute ([("@",Lit s),("^",Lit $ unwords $ concat deps)] ++ [("<",Lit d) | d:_ <- [concat deps]])
                subs v xs = concat [if x == '%' then v else [x] | x <- xs]
                (deps,cmds) = unzip [ (words $ subs v $ flat prerequisites, map (subs v . flat) commands)
                                    | r@Rule{..} <- rs, Just v <- [check s r]]
        match s = any (isJust . check s) rs


flatten :: Expr -> String
flatten (Lit x) = x
flatten x = error $ "Could not flatten: " ++ show x


runCommand :: String -> Action ()
runCommand x = traced (unwords $ take 1 $ words x) $ do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "System command failed: " ++ x


makePattern :: String -> FilePath -> Maybe FilePath
makePattern pat v = case break (== '%') pat of
    (pre,'%':post) -> let rest = length v - (length pre + length post) in
                      if pre `isPrefixOf` v && post `isSuffixOf` v && rest >= 0
                      then Just $ take rest $ drop (length pre) v else Nothing
    otherwise -> if pat == v then Just "" else Nothing
