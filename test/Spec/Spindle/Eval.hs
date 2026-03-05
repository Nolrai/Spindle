{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Eval (evaluaterTests) where

import Spindle.Eval
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Spindle.Parser (myParser, myParse)

evaluaterTests :: TestTree
evaluaterTests = testGroup "evaluator Tests"
  [ testCase "eval literal" $
    (eval <$> myParse myParser "42" "42") @?= pure (pure 42)
  , testCase "eval simple expression" $
    (eval <$> myParse myParser "simple expression" "1 + 2") @?= pure (pure 3)
  , testCase "eval unary minus" $
    (eval <$> myParse myParser "unary minus" "-5") @?= pure (pure (-5))
  , testCase "eval increment" $
    (eval <$> myParse myParser "increment" "3++") @?= pure (pure 4)
  , testCase "eval decrement" $
    (eval <$> myParse myParser "decrement" "4--") @?= pure (pure 3)
  , testCase "eval precedence" $
    (eval <$> myParse myParser "precedence" "2 + 3 * 4") @?= pure (pure 14)
  , testCase "eval precedence with parens" $
    (eval <$> myParse myParser "precedence with parens" "(2 + 3) * 4") @?= pure (pure 20)
  , testCase "eval division and subtraction" $
    (eval <$> myParse myParser "division and subtraction" "8 / 2 - 1") @?= pure (pure 3)
  , testCase "eval complex nesting" $
    (eval <$> myParse myParser "complex nesting" "1 + (2 * (3 + 4))") @?= pure (pure 15)
  , testCase "eval chained operations" $
    (eval <$> myParse myParser "chained operations" "1 + 2 + 3 + 4") @?= pure (pure 10)
  , testCase "eval conditional true branch" $
    (eval <$> myParse myParser "conditional true branch" "(1 ? 2 : 3)") @?= pure (pure 2)
  , testCase "eval conditional false branch" $
    (eval <$> myParse myParser "conditional false branch" "(0 ? 2 : 3)") @?= pure (pure 3)
  , testCase "eval nested conditional" $
    (eval <$> myParse myParser "nested conditional" "(1 ? (0 ? 4 : 5) : 3)") @?= pure (pure 5)
  , testCase "eval conditional with expression branches" $
    (eval <$> myParse myParser "conditional with expression branches" "(1 ? 2 + 3 : 4 * 5)") @?= pure (pure 5)
  , testCase "eval let binding" $
    (eval <$> myParse myParser "let binding" "let v := 2 in v") @?= pure (pure 2)
  , testCase "eval let binding with expression" $
    (eval <$> myParse myParser "let binding with expression" "let x := 1 + 2 in x + 3") @?= pure (pure 6)
  , testCase "eval nested let bindings" $
    (eval <$> myParse myParser "nested let bindings" "let x := 2 in let y := x + 3 in y * 2") @?= pure (pure 10)
  , testCase "eval let shadowing" $
    (eval <$> myParse myParser "let shadowing" "let x := 1 in let x := 2 in x + x") @?= pure (pure 4)
  ]
