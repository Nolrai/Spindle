{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Eval (evaluaterTests) where

import Spindle.Eval
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Spindle.Parser (myParser, myParse)
import Spindle.Types

evaluaterTests :: TestTree
evaluaterTests = testGroup "evaluator Tests"
  [ simpleConstructorTests
  , precedenceTests
  , evalCondTests
  , letTests
  ]

-- | Tests for simple expression constructors, to make sure they are being evaluated correctly
simpleConstructorTests :: TestTree
simpleConstructorTests = testGroup "expr constructor tests"
  [ testCase "eval literal" $
    (eval <$> myParse myParser "42" "42") @?= pure (pure (NLit 42))

  , testCase "eval addition" $
    (eval <$> myParse myParser "addition" "1 + 2") @?= pure (pure (NLit 3))
  , testCase "eval multiplication" $
    (eval <$> myParse myParser "multiplication" "3 * 4") @?= pure (pure (NLit 12))
  , testCase "eval division" $
    (eval <$> myParse myParser "division" "8 / 2") @?= pure (pure (NLit 4))
  , testCase "eval division by zero" $
    (eval <$> myParse myParser "division by zero" "8 / 0") @?= pure (pure (NLit 0))
  , testCase "eval subtraction" $
    (eval <$> myParse myParser "subtraction" "5 - 2") @?= pure (pure (NLit 3))

  , testCase "eval unary minus" $
    (eval <$> myParse myParser "unary minus" "-5") @?= pure (pure (NLit (-5)))
  , testCase "eval increment" $
    (eval <$> myParse myParser "increment" "3++") @?= pure (pure (NLit 4))
  , testCase "eval decrement" $
    (eval <$> myParse myParser "decrement" "4--") @?= pure (pure (NLit 3))
  ]

precedenceTests :: TestTree
precedenceTests = testGroup "precedence tests"
  [ testCase "eval precedence 1" $
    (eval <$> myParse myParser "precedence 1" "1 + 2 * 3") @?= pure (pure (NLit 7))
  , testCase "eval precedence 2" $
    (eval <$> myParse myParser "precedence 2" "4 * 5 - 6") @?= pure (pure (NLit 14))
  , testCase "eval precedence 3" $
    (eval <$> myParse myParser "precedence 3" "8 / 4 + 2") @?= pure (pure (NLit 4))
  , testCase "eval precedence 4" $
    (eval <$> myParse myParser "precedence 4" "3 + 4 * 2 - 1") @?= pure (pure (NLit 10))
  , testCase "eval precedence 5" $
    (eval <$> myParse myParser "precedence 5" "6 - 2 * 3 + 4") @?= pure (pure (NLit 4))
  , testCase "eval precedence 6" $
    (eval <$> myParse myParser "precedence 6" "8 / 2 * 3") @?= pure (pure (NLit 12))
  , testCase "eval precedence 7" $
    (eval <$> myParse myParser "precedence 7" "5 + 6 - 7 * 8 / 4") @?= pure (pure (NLit (-3)))
  , testCase "eval precedence 8" $
    (eval <$> myParse myParser "precedence 8" "2 * 3 + 4 / 2 - 1") @?= pure (pure (NLit 7))
  , testCase "eval precedence" $
    (eval <$> myParse myParser "precedence" "2 + 3 * 4") @?= pure (pure (NLit 14))
  , testCase "eval precedence with parens" $
    (eval <$> myParse myParser "precedence with parens" "(2 + 3) * 4") @?= pure (pure (NLit 20))
  , testCase "eval division and subtraction" $
    (eval <$> myParse myParser "division and subtraction" "8 / 2 - 1") @?= pure (pure (NLit 3))
  , testCase "eval complex nesting" $
    (eval <$> myParse myParser "complex nesting" "1 + (2 * (3 + 4))") @?= pure (pure (NLit 15))
  , testCase "eval chained operations" $
    (eval <$> myParse myParser "chained operations" "1 + 2 + 3 + 4") @?= pure (pure (NLit 10))
  ]

evalCondTests :: TestTree
evalCondTests = testGroup "conditional tests"
  [ testCase "eval conditional true branch" $
    (eval <$> myParse myParser "conditional true branch" "(1 ? 2 : 3)") @?= pure (pure (NLit 2))
  , testCase "eval conditional false branch" $
    (eval <$> myParse myParser "conditional false branch" "(0 ? 2 : 3)") @?= pure (pure (NLit 3))
  , testCase "eval nested conditional" $
    (eval <$> myParse myParser "nested conditional" "(1 ? (0 ? 4 : 5) : 3)") @?= pure (pure (NLit 5))
  , testCase "eval conditional with expression branches" $
    (eval <$> myParse myParser "conditional with expression branches" "(1 ? 2 + 3 : 4 * 5)") @?= pure (pure (NLit 5))
  , testCase "eval conditional with nested expressions" $
    (eval <$> myParse myParser "conditional with nested expressions" "(1 ? (2 + 3) * 4 : 5 - 6)") @?= pure (pure (NLit 20))
  ]

letTests :: TestTree
letTests = testGroup "let binding tests"
  [ testCase "eval let binding" $
    (eval <$> myParse myParser "let binding" "let v := 2 in v") @?= pure (pure (NLit 2))
  , testCase "eval let binding with expression" $
    (eval <$> myParse myParser "let binding with expression" "let x := 1 + 2 in x + 3") @?= pure (pure (NLit 6))
  , testCase "eval nested let bindings" $
    (eval <$> myParse myParser "nested let bindings" "let x := 2 in let y := x + 3 in y * 2") @?= pure (pure (NLit 10))
  , testCase "eval let shadowing" $
    (eval <$> myParse myParser "let shadowing" "let x := 1 in let x := 2 in x + x") @?= pure (pure (NLit 4))
  ]
