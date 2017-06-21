module Test.Existence(main) where

import           Development.Shake                   (getDirectoryFilesIO)
import           Development.Shake.Internal.FileInfo (getFileInfo)
import           Development.Shake.Internal.FileName (fileNameFromString)
import           System.Directory                    (getCurrentDirectory)
import           System.FilePath                     ((</>))

main :: IO () -> IO ()
main _ = do
    cwd <- getCurrentDirectory
    someFiles <- getDirectoryFilesIO cwd ["*"]
    let someFile = head someFiles
    assertIsJust . getFileInfo $ fileNameFromString someFile

    let fileThatCantExist = someFile </> "fileThatCantExist"
    assertIsNothing . getFileInfo $ fileNameFromString fileThatCantExist

assertIsJust :: IO (Maybe a) -> IO ()
assertIsJust action = do
    Just _ <- action
    return ()

assertIsNothing :: IO (Maybe a) -> IO ()
assertIsNothing action = do
    Nothing <- action
    return ()
