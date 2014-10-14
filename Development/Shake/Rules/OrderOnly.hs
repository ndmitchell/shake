{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.Rules.OrderOnly(
     defaultRuleOrderOnly, orderOnly, orderOnlyBS
    ) where

import Development.Shake.Core
import General.String
import Development.Shake.Classes
import Development.Shake.Types
import Development.Shake.Rules.File
import qualified Data.ByteString.Char8 as BS


newtype OrderOnlyQ = OrderOnlyQ BSU
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show OrderOnlyQ where show (OrderOnlyQ x) = unpackU x

newtype OrderOnlyA = OrderOnlyA ()
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show OrderOnlyA where show (OrderOnlyA ()) = "OrderOnly"


instance Rule OrderOnlyQ OrderOnlyA where
    storedValue _ (OrderOnlyQ _) = return $ Just $ OrderOnlyA ()


defaultRuleOrderOnly :: Rules ()
defaultRuleOrderOnly = rule $ \(OrderOnlyQ x) -> Just $ do
    needBS [unpackU_ x]
    return $ OrderOnlyA ()


-- | Define order-only dependencies, these are dependencies that will always
--   be built before continuing, but which aren't dependencies of this action.
--   Mostly useful for defining generated dependencies you think might be real dependencies.
--   If they turn out to be real dependencies, you should add an explicit dependency afterwards.
--
-- @
-- \"source.o\" *> \\out -> do
--     'orderOnly' \"header.h\"
--     () <- 'cmd' \"gcc -c source.c -o source.o -MMD -MF source.m\"
--     'neededMakefileDependencies' \"source.m\"
-- @
--
--   If @header.h@ is included by @source.c@ then the call to 'needMakefileDependencies' will cause
--   it to be added as a real dependency. If it isn't, then the rule won't rebuild if it changes,
--   and you will have lost some opportunity for parallelism.
orderOnly :: Targets targets => targets -> Action ()
orderOnly targets = (apply $ map (OrderOnlyQ . packU) $ filePaths targets :: Action [OrderOnlyA]) >> return ()


orderOnlyBS :: [BS.ByteString] -> Action ()
orderOnlyBS xs = (apply $ map (OrderOnlyQ . packU_) xs :: Action [OrderOnlyA]) >> return ()
