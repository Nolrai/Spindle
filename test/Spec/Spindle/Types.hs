{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Types (typesTests) where

import Spindle.Types
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)

typesTests :: TestTree
typesTests = testGroup "Types Tests"
  [ testCase "Expr constructors" $
      let e1 = Lit 42
          e2 = BiOp Add (Lit 1) (Lit 2)
          e3 = UnOp Neg (Lit 5)
          e4 = Cond (Lit 1) (Lit 2) (Lit 3)
          e5 = Let "x" (Lit 10) (Var "x")
          e6 = Var "y"
      in do
        e1 @?= Lit 42
        e2 @?= BiOp Add (Lit 1) (Lit 2)
        e3 @?= UnOp Neg (Lit 5)
        e4 @?= Cond (Lit 1) (Lit 2) (Lit 3)
        e5 @?= Let "x" (Lit 10) (Var "x")
        e6 @?= Var "y"
  , testCase "BiOp constructors" $
      let op1 = Add
          op2 = Sub
          op3 = Mul
          op4 = Div
      in do
        op1 @?= Add
        op2 @?= Sub
        op3 @?= Mul
        op4 @?= Div
  , testCase "UnOp constructors" $
      let op1 = Neg
          op2 = Inc
          op3 = Dec
      in do
        op1 @?= Neg
        op2 @?= Inc
        op3 @?= Dec
  ]
