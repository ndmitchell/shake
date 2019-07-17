{-# LANGUAGE CPP #-}

-- | Working with escape sequences
module General.EscCodes(
    Color(..),
    checkEscCodes,
    removeEscCodes,
    escWindowTitle,
    escCursorUp,
    escClearLine,
    escForeground,
    escBold,
    escNormal
    ) where

import Data.Char
import System.IO
import System.Environment
import System.IO.Unsafe

#ifdef mingw32_HOST_OS
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
#endif

checkEscCodes :: IO Bool
checkEscCodes = return checkEscCodesOnce

{-# NOINLINE checkEscCodesOnce #-}
checkEscCodesOnce :: Bool
checkEscCodesOnce = unsafePerformIO $ do
    hdl <- hIsTerminalDevice stdout
    env <- maybe False (/= "dumb") <$> lookupEnv "TERM"
    if hdl && env then return True else
#ifdef mingw32_HOST_OS
        checkEscCodesWindows
#else
        return False
#endif

#ifdef mingw32_HOST_OS

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "Windows.h GetStdHandle" c_GetStdHandle :: Word32 -> IO (Ptr ())
foreign import CALLCONV unsafe "Windows.h GetConsoleMode" c_GetConsoleModule :: Ptr () -> Ptr Word32 -> IO Bool
foreign import CALLCONV unsafe "Windows.h SetConsoleMode" c_SetConsoleMode :: Ptr () -> Word32 -> IO Bool

c_STD_OUTPUT_HANDLE = 4294967285 :: Word32 -- (-11) for some reason
c_ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004 :: Word32


-- | Try and get the handle attributes, if they are all satisifed, return True.
--   If they aren't, try and set it to emulated mode.
checkEscCodesWindows :: IO Bool
checkEscCodesWindows = do
    h <- c_GetStdHandle c_STD_OUTPUT_HANDLE
    -- might return INVALID_HANDLE_VALUE, but then the next step will happily fail
    mode <- alloca $ \v -> do
        b <- c_GetConsoleModule h v
        if b then Just <$> peek v else return Nothing
    case mode of
        Nothing -> return False
        Just mode -> do
            let modeNew = mode .|. c_ENABLE_VIRTUAL_TERMINAL_PROCESSING
            if mode == modeNew then return True else do
                c_SetConsoleMode h modeNew
#endif

removeEscCodes :: String -> String
removeEscCodes ('\ESC':'[':xs) = removeEscCodes $ drop 1 $ dropWhile (not . isAlpha) xs
removeEscCodes (x:xs) = x : removeEscCodes xs
removeEscCodes [] = []


escWindowTitle :: String -> String
escWindowTitle x = "\ESC]0;" ++ x ++ "\BEL"

escCursorUp :: Int -> String
escCursorUp i = "\ESC[" ++ show i ++ "A"

escClearLine :: String
escClearLine = "\ESC[K"

escBold :: String
escBold = "\ESC[1m"

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Show,Enum)

escForeground :: Color -> String
escForeground x = "\ESC[" ++ show (30 + fromEnum x) ++ "m"

escNormal :: String
escNormal = "\ESC[0m"
