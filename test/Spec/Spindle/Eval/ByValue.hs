{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Eval.ByValue (byValueTests) where

import Spindle.Eval.ByValue
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Spindle.Parser (myParser)
import Spindle.Expr
import Utils
import Data.Text (Text)
import GHC.IsList (fromList)
import Spindle.Eval.Common

byValueTests :: TestTree
byValueTests = testGroup "ByValue evaluator Tests"
  [ simpleConstructorTests
  , precedenceTests
  , evalCondTests
  , boolExprTests
  , letTests
  , lamAppTests
  ]

-- The following test groups are adapted from the original Eval.hs
-- They use the ByValue evaluator

boolExprTests :: TestTree
boolExprTests = testGroup "boolean expression tests (ByValue)"
  [ evalTestCase "bool literal true" "T" (Right (NBLit True))
  , evalTestCase "bool literal false" "F" (Right (NBLit False))
  , evalTestCase "and true true" "T && T" (Right (NBLit True))
  , evalTestCase "and true false" "T && F" (Right (NBLit False))
  , evalTestCase "or false false" "F || F" (Right (NBLit False))
  , evalTestCase "or true false" "T || F" (Right (NBLit True))
  , evalTestCase "not true" "!T" (Right (NBLit False))
  , evalTestCase "not false" "!F" (Right (NBLit True))
  , evalTestCase "equality true" "T == T" (Right (NBLit True))
  , evalTestCase "equality false" "T == F" (Right (NBLit False))
  , evalTestCase "inequality true" "T != F" (Right (NBLit True))
  , evalTestCase "inequality false" "F != F" (Right (NBLit False))
  , evalTestCase "less than true" "1 < 2" (Right (NBLit True))
  , evalTestCase "less than false" "2 < 1" (Right (NBLit False))
  , evalTestCase "greater than true" "3 > 2" (Right (NBLit True))
  , evalTestCase "greater than false" "2 > 3" (Right (NBLit False))
  , evalTestCase "less or equal true" "2 <= 2" (Right (NBLit True))
  , evalTestCase "less or equal false" "3 <= 2" (Right (NBLit False))
  , evalTestCase "greater or equal true" "3 >= 2" (Right (NBLit True))
  , evalTestCase "greater or equal false" "1 >= 2" (Right (NBLit False))
  , evalTestCase "complex boolean expr" "(1 < 2) && (3 > 2) || F" (Right (NBLit True))
  ]

evalTestCase :: String -> Text -> Either Err Thunk -> TestTree
evalTestCase name input expected =
  testCase name $
    (eval <$> myParse myParser name input) ?= expected

simpleConstructorTests :: TestTree
simpleConstructorTests = testGroup "expr constructor tests (ByValue)"
  [ evalTestCase "literal" "42" (Right (NILit 42))
  , evalTestCase "addition" "1 + 2" (Right (NILit 3))
  , evalTestCase "multiplication" "3 * 4" (Right (NILit 12))
  , evalTestCase "division" "8 / 2" (Right (NILit 4))
  , evalTestCase "division by zero" "8 / 0" (Right (NILit 0))
  , evalTestCase "subtraction" "5 - 2" (Right (NILit 3))
  , evalTestCase "unary minus" "-5" (Right (NILit (-5)))
  , evalTestCase "increment" "3++" (Right (NILit 4))
  , evalTestCase "decrement" "4--" (Right (NILit 3))
  ]

precedenceTests :: TestTree
precedenceTests = testGroup "precedence tests (ByValue)"
  [ evalTestCase "precedence 1" "1 + 2 * 3" (Right (NILit 7))
  , evalTestCase "precedence 2" "4 * 5 - 6" (Right (NILit 14))
  , evalTestCase "precedence 3" "8 / 4 + 2" (Right (NILit 4))
  , evalTestCase "precedence 4" "3 + 4 * 2 - 1" (Right (NILit 10))
  , evalTestCase "precedence 5" "6 - 2 * 3 + 4" (Right (NILit 4))
  , evalTestCase "precedence 6" "8 / 2 * 3" (Right (NILit 12))
  , evalTestCase "precedence 7" "5 + 6 - 7 * 8 / 4" (Right (NILit (-3)))
  , evalTestCase "precedence 8" "2 * 3 + 4 / 2 - 1" (Right (NILit 7))
  , evalTestCase "precedence" "2 + 3 * 4" (Right (NILit 14))
  , evalTestCase "precedence with parens" "(2 + 3) * 4" (Right (NILit 20))
  , evalTestCase "division and subtraction" "8 / 2 - 1" (Right (NILit 3))
  , evalTestCase "complex nesting" "1 + (2 * (3 + 4))" (Right (NILit 15))
  , evalTestCase "chained operations" "1 + 2 + 3 + 4" (Right (NILit 10))
  ]

evalCondTests :: TestTree
evalCondTests = testGroup "conditional tests (ByValue)"
  [ evalTestCase "conditional true branch" "(T ? 2 : 3)" (Right (NILit 2))
  , evalTestCase "conditional false branch" "(F ? 2 : 3)" (Right (NILit 3))
  , evalTestCase "nested conditional" "(T ? (F ? 4 : 5) : 3)" (Right (NILit 5))
  , evalTestCase "conditional with expression branches" "(T ? 2 + 3 : 4 * 5)" (Right (NILit 5))
  , evalTestCase "conditional with nested expressions" "(T ? (2 + 3) * 4 : 5 - 6)" (Right (NILit 20))
  ]

lamAppTests :: TestTree
lamAppTests = testGroup "lambda application tests (ByValue)"
  [ evalTestCase "simple lambda application" "(\\ x => x + 1) # 5" (Right (NILit 6))
  , evalTestCase "lambda application with multiple parameters" "(\\ x y => x * y) # 3 # 4" (Right (NILit 12))
  , evalTestCase "lambda application with nested lambdas" "(  \\ x => \\ y => x + y) # 2 # 3" (Right (NILit 5))
  , evalTestCase "lambda application with excess arguments" "(\\ x => x + 1) # 3 # 6" (Left (NotLambda (NILit 4)))
  , evalTestCase "lambda application with excess parameters" "(\\ x y => x + y) # 2" (Right (NClosure (fromList [("x", NILit 2)]) ["y"] (BiOp (ArithOp Add) (Var "x") (Var "y"))))
  , evalTestCase "closure captures defining environment" alphaRenamingText1 (Right (NILit 42))
  , evalTestCase "inner binder does not capture closure variable" alphaRenamingText2 (Right (NILit 42))
  , evalTestCase "later shadowing does not affect closure variable" alphaRenamingText3 (Right (NILit 5))
  ]

alphaRenamingText1 :: Text
alphaRenamingText1 =
  "let z := 42 in"
  <> "let y := (\\w => z) in"
  <> "let f := ((\\x => \\z => x) # y) in"
  <> "(f # 0) # 999"

alphaRenamingText2 :: Text
alphaRenamingText2 =
  "let z := 42 in"
  <> "let y := (\\w => z) in"
  <> "(\\x => \\z => x) # y # 0 # 999"

alphaRenamingText3 :: Text
alphaRenamingText3 =
  "let a := 5 in"
  <> "let f := (\\u => a) in"
  <> "(((\\x => \\a => x) # f) # 99) # 0"

letTests :: TestTree
letTests = testGroup "let binding tests (ByValue)"
  [ evalTestCase "eval let binding" "let v := 2 in v" (Right (NILit 2))
  , evalTestCase "eval let binding with expression" "let x := 1 + 2 in x + 3" (Right (NILit 6))
  , evalTestCase "eval nested let bindings" "let x := 2 in let y := x + 3 in y * 2" (Right (NILit 10))
  , evalTestCase "eval let shadowing" "let x := 1 in let x := 2 in x + x" (Right (NILit 4))
  ]
