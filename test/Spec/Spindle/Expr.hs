{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Expr (typesTests) where

import Spindle.Expr
import Test.Tasty
import Test.Tasty.HUnit

typesTests :: TestTree
typesTests = testGroup "Types Tests"
    [ testCase "ILit constructor" $
        ILit 42 @?= ILit 42
    , testCase "BLit constructor" $
        BLit True @?= BLit True
    , testCase "Var constructor" $
        Var "x" @?= Var "x"
    , testCase "Lam constructor" $
        Lam ["x"] (Var "x") @?= Lam ["x"] (Var "x")
    , testCase "App constructor" $
        App (Var "f") [Var "x"] @?= App (Var "f") [Var "x"]
    ]

