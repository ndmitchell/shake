{-# LANGUAGE PatternGuards #-}

module Test.Oracle(main) where

import Development.Shake
import Test.Type
import Control.Monad


main = shaken test $ \args obj -> do
    let f name lhs rhs = (,) name $
            (do addOracle $ \k -> let _ = k `asTypeOf` lhs in return rhs; return ()
            ,let o = obj name ++ ".txt" in do want [o]; o %> \_ -> do v <- askOracleWith lhs rhs; writeFile' o $ show v)
    let tbl = [f "str-bool" "" True
              ,f "str-int" "" (0::Int)
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

    {-
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
    build ["@foo","%newer","+str-int"]
    assertContents (obj "unit.txt") "test"
    build ["@foo","%newer"]
    assertContents (obj "unit.txt") "test"

    -- check always run works
    build ["!foo"]
    assertContents (obj "rerun") "foo"
    build ["!bar"]
    assertContents (obj "rerun") "bar"
    -}

    -- check error messages are good
    let errors args err = assertException [err] $ build $ "--quiet" : args

    errors ["*str-bool"] -- Building with an Oracle that I know nothing about
        "missing a call to addOracle"

    build ["+str-int","*str-int"]
    errors ["*str-int"] -- Building with an an Oracle that has been removed
        "missing a call to addOracle"
    putStrLn "success"

    {-
    build ["+str-int","*str-int"]
    errors ["+str-bool","*str-int"] -- Building with an Oracle that has changed type
        "askOracle is used at the wrong type"

    errors ["+str-int","+str-bool"] -- Two Oracles with the same question type
        "Only one call to addOracle is allowed"

    errors ["+str-int","*str-bool"] -- Using an Oracle at the wrong answer type
        "askOracle is used at the wrong type"

    build ["+str-int","+str-int"] -- Two Oracles work if they aren't used
    errors ["+str-int","+str-int","*str-int"] -- Two Oracles fail if they are used
        "Only one call to addOracle is allowed"

    errors ["+str-int","+str-bool"] -- Two Oracles with the same answer type
        "Only one call to addOracle is allowed"
    -}
