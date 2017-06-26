-- | This module provides functions for defining memoized build rules. Memoized
-- build rules save their results in a persistent store, and re-use them when
-- the exact same build is requested later. Two builds are considered the same
-- when they have the same set of dependencies, and the contents of the
-- dependencies (e.g. the file contents) match. The same store can be shared
-- between multiple build directories.
--
-- In order to enable memoized rules, either use the @--memo-store@ option, or
-- override 'Development.Shake.shakeMemoSave' and
-- 'Development.Shake.shakeMemoRestore' in 'Development.Shake.ShakeOptions'.
module Development.Shake.Memo(
    memoFiles, hashState,
    -- * A filesystem-based reference implementation of the memo API
    fsMemoRestore, fsMemoSave,
    ) where

import Development.Shake.Internal.Memo
