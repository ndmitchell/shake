{-# LANGUAGE PatternGuards, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Test.Oracle(main) where

import Development.Shake
import Development.Shake.Classes
import General.GetOpt
import Test.Type hiding (RandomType)
import qualified Test.Type as T
import Control.Monad

type instance RuleResult String = Bool
type instance RuleResult Bool = String
type instance RuleResult Int = String
type instance RuleResult () = String

newtype RandomType = RandomType (BinarySentinel String)
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)

type instance RuleResult RandomType = Int
type instance RuleResult T.RandomType = Int

data Opt = Plus String | Star String | At String | Perc String
opts = [f "plus" Plus, f "star" Star, f "at" At, f "perc" Perc]
    where f s con = Option "" [s] (ReqArg (Right . con) "") ""

main = shakeTest test opts $ \args -> do
    addOracle $ \(T.RandomType _) -> return 42
    addOracle $ \(RandomType _) -> return (-42)

    "randomtype.txt" %> \out -> do
        a <- askOracle $ T.RandomType $ BinarySentinel ()
        b <- askOracle $ RandomType $ BinarySentinel ()
        writeFile' out $ show (a,b)

    let f :: (ShakeValue q, ShakeValue (RuleResult q)) => String -> q -> RuleResult q -> (String, (Rules (), Rules ()))
        f name lhs rhs = (,) name
            (do addOracle $ \k -> let _ = k `asTypeOf` lhs in return rhs; return ()
            ,let o = name ++ ".txt" in do want [o]; o %> \_ -> do v <- askOracle lhs; writeFile' o $ show v)
    let tbl = [f "str-bool" "" True
              -- ,f "str-int" "" (0::Int)
              ,f "bool-str" True ""
              ,f "int-str" (0::Int) ""]

    forM_ args $ \a -> case a of
        Plus x | Just (add,_) <- lookup x tbl -> add
        Star x | Just (_,use) <- lookup x tbl -> use
        At key -> do addOracle $ \() -> return key; return ()
        Perc name -> let o = "unit.txt" in do want [o]; o %> \_ -> do {askOracle (); writeFile' o name}

test build = do
    build ["clean"]

    build ["randomtype.txt"]
    assertContents "randomtype.txt" "(42,-42)"

    -- check it rebuilds when it should
    build ["--at=key","--perc=name"]
    assertContents "unit.txt" "name"
    build ["--at=key","--perc=test"]
    assertContents "unit.txt" "name"
    build ["--at=foo","--perc=test"]
    assertContents "unit.txt" "test"

    -- check adding/removing redundant oracles does not trigger a rebuild
    build ["--at=foo","--perc=newer","--plus=str-bool"]
    assertContents "unit.txt" "test"
    build ["--at=foo","--perc=newer","--plus=int-str"]
    assertContents "unit.txt" "test"
    build ["--at=foo","--perc=newer"]
    assertContents "unit.txt" "test"

    -- check error messages are good
    let errors args err = assertException [err] $ build $ "--quiet" : args

    build ["--plus=str-bool","--star=str-bool"]
    errors ["--star=str-bool"] -- Building with an an Oracle that has been removed
        "missing a call to addOracle"

    errors ["--star=bool-str"] -- Building with an Oracle that I know nothing about
        "missing a call to addOracle"

{-
    build ["+str-int","*str-int"]
    errors ["+str-bool","*str-int"] -- Building with an Oracle that has changed type
        "askOracle is used at the wrong type"

    errors ["+str-int","+str-bool"] -- Two Oracles with the same question type
        "Internal error" -- TODO: "Only one call to addOracle is allowed"

    errors ["+str-int","*str-bool"] -- Using an Oracle at the wrong answer type
        "askOracle is used at the wrong type"
-}
    errors ["--plus=str-bool","--plus=str-bool"] -- Two Oracles work if they aren't used
        "" -- TODO: Should they?
    errors ["--plus=str-bool","--plus=str-bool","--star=str-bool"] -- Two Oracles fail if they are used
        "Internal error" -- TODO!: "Only one call to addOracle is allowed"
