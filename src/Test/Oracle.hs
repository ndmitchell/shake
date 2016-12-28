{-# LANGUAGE PatternGuards, FlexibleContexts, TypeFamilies, ConstraintKinds, GeneralizedNewtypeDeriving #-}

module Test.Oracle(main) where

import Development.Shake
import Development.Shake.Classes
import Test.Type hiding (RandomType)
import qualified Test.Type as T
import Control.Monad

type instance RuleResult String = Bool
type instance RuleResult Bool = String
type instance RuleResult Int = String
type instance RuleResult () = String

newtype RandomType = RandomType (BinarySentinel String)
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)

type instance RuleResult RandomType = ()
type instance RuleResult T.RandomType = ()

main = shakenCwd test $ \args obj -> do
    addOracle $ \(T.RandomType _) -> return ()
    addOracle $ \(RandomType _) -> return ()
    action $ do
        askOracle $ T.RandomType $ BinarySentinel ()
        askOracle $ RandomType $ BinarySentinel ()

    let f :: (ShakeValue q, ShakeValue (RuleResult q)) => String -> q -> RuleResult q -> (String, (Rules (), Rules ()))
        f name lhs rhs = (,) name
            (do addOracle $ \k -> let _ = k `asTypeOf` lhs in return rhs; return ()
            ,let o = obj name ++ ".txt" in do want [o]; o %> \_ -> do v <- askOracleWith lhs rhs; writeFile' o $ show v)
    let tbl = [f "str-bool" "" True
              -- ,f "str-int" "" (0::Int)
              ,f "bool-str" True ""
              ,f "int-str" (0::Int) ""]

    forM_ args $ \a -> case a of
        '+':x | Just (add,_) <- lookup x tbl -> add
        '*':x | Just (_,use) <- lookup x tbl -> use
        '@':key -> do addOracle $ \() -> return key; return ()
        '%':name -> let o = obj "unit.txt" in do want [o]; o %> \_ -> do {askOracleWith () ""; writeFile' o name}
        '!':name -> do want [obj "rerun"]; obj "rerun" %> \out -> do alwaysRerun; writeFile' out name

test build obj = do
    build ["clean"]

    -- check it rebuilds when it should
    build ["@key","%name"]
    assertContents (obj "unit.txt") "name"
    build ["@key","%test"]
    assertContents (obj "unit.txt") "name"
    build ["@foo","%test"]
    assertContents (obj "unit.txt") "test"

    -- check adding/removing redundant oracles does not trigger a rebuild
    build ["@foo","%newer","+str-bool"]
    assertContents (obj "unit.txt") "test"
    build ["@foo","%newer","+int-str"]
    assertContents (obj "unit.txt") "test"
    build ["@foo","%newer"]
    assertContents (obj "unit.txt") "test"

    -- check always run works
    build ["!foo"]
    assertContents (obj "rerun") "foo"
    build ["!bar"]
    assertContents (obj "rerun") "bar"

    -- check error messages are good
    let errors args err = assertException [err] $ build $ "--quiet" : args

    build ["+str-bool","*str-bool"]
    errors ["*str-bool"] -- Building with an an Oracle that has been removed
        "missing a call to addOracle"

    errors ["*bool-str"] -- Building with an Oracle that I know nothing about
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
    errors ["+str-bool","+str-bool"] -- Two Oracles work if they aren't used
        "" -- TODO: Should they?
    errors ["+str-bool","+str-bool","*str-bool"] -- Two Oracles fail if they are used
        "Internal error" -- TODO: "Only one call to addOracle is allowed"

{-
    errors ["+str-int","+str-bool"] -- Two Oracles with the same answer type
        "Internal error" -- TODO: "Only one call to addOracle is allowed"
-}
