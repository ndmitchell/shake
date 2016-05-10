{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Development.Shake.Experiment.Rules where

import Development.Shake.Experiment.Interface
import Development.Shake.Classes
import Development.Shake.Core
import Data.Proxy
import System.Directory
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS


---------------------------------------------------------------------
-- SUPER-FAST STRINGS

newtype SpecialString = SpecialString BS.ByteString
    deriving (Encoder, Hashable, Typeable, Eq)

packSpecialString :: String -> SpecialString
packSpecialString = undefined

unpackSpecialString :: SpecialString -> String
unpackSpecialString = undefined

instance Encoder String where
    encode = BS.pack
    decode = BS.unpack

instance Encoder Int where
    encode = encode . show
    decode = read . decode


---------------------------------------------------------------------
-- ALWAYS RERUN

newtype AlwaysRerun = AlwaysRerun ()
    deriving (Eq, Hashable, Encoder, Typeable)

addAlwaysRerun = addBuiltinRule $ \_ _ AlwaysRerun{} _ _ -> return (False, BS.empty, False, (), True)


---------------------------------------------------------------------
-- PHONY

data Phony = Phony String (Action ())
           | Phonys (String -> Maybe (Action ())) 

isPhony :: String -> Phony -> Maybe (Action ())
isPhony s (Phony s2 a) | s == s2 = Just a
isPhony s (Phonys p) | Just a <- p s = Just a
isPhony _ _ = Nothing

addPhony = addBuiltinRule $ \_ ask ->
    let rules = matchUserRule $ ask (Proxy :: Proxy Phony)
    in \key _ _ -> case rules $ isPhony key of
            [] -> error "no match"
            _:_:_ -> error "multiple match"
            [x] -> do
                x
                return (True, mempty, True, (), True)


---------------------------------------------------------------------
-- DIRECTORY RULES

newtype DoesFileExist = DoesFileExist String
    deriving (Eq, Hashable, Encoder, Typeable)

addDoesFileExist = addBuiltinRule $ \_ _ (DoesFileExist x) old _ -> do
    b <- liftIO $ doesFileExist x
    let diff = maybe True ((/=) b . decode) old
    return (False, encode b, diff, b, diff)


newtype DirectoryContents = DirectoryContents String
    deriving (Eq, Hashable, Encoder, Typeable)

addDirectoryContents = addBuiltinRule $ \_ _ (DirectoryContents x) old _ -> do
    xs <- liftIO $ getDirectoryContents x
    let h = hash xs
    let diff = maybe True ((/=) h . decode) old
    return (False, encode h, diff, xs, diff)
