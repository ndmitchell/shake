
-- | Get terminal size with @ioctl@
module Development.Shake.Internal.TermSize (
    getTermSize
    ) where

#ifdef WIN32
getTermSize :: IO (Int, Int)
getTermSize = return (25,80)
#else

import Foreign
import Foreign.C.Error
import Foreign.C.Types

#include <sys/ioctl.h>
#include <unistd.h>

-- Trick for calculating alignment of a type, taken from
-- http://www.haskell.org/haskellwiki/FFICookBook#Working_with_structs
#let our_alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- The ws_xpixel and ws_ypixel fields are unused, so I've omitted them here.
data WinSize = WinSize { wsRow, wsCol :: CUShort }

instance Storable WinSize where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#our_alignment struct winsize)
  peek ptr = do
    row <- (#peek struct winsize, ws_row) ptr
    col <- (#peek struct winsize, ws_col) ptr
    return $ WinSize row col
  poke ptr (WinSize row col) = do
    (#poke struct winsize, ws_row) ptr row
    (#poke struct winsize, ws_col) ptr col

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

-- | Return current number of (rows, columns) of the terminal.
getTermSize :: IO (Int, Int)
getTermSize =
  with (WinSize 0 0) $ \ws -> do
    throwErrnoIfMinus1 "ioctl" $
      ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
    WinSize row col <- peek ws
    return (fromIntegral row, fromIntegral col)
#endif
