{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Expr (exprTests) where

import Spindle.Expr
import Test.Tasty
import Test.Tasty.HUnit

import Hedgehog (Property, forAll, property, (===), MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as Text
import Test.Tasty.Hedgehog

-- HUnit tests for Expr construction and equality
unit_expr_constructors :: TestTree
unit_expr_constructors = testGroup "Expr constructors and Eq"
  [ testCase "ILit equality" $ ILit 42 @?= ILit 42
  , testCase "BLit equality" $ BLit True @?= BLit True
  , testCase "Var equality" $ Var "x" @?= Var "x"
  , testCase "Lam equality" $ Lam ["x"] (Var "x") @?= Lam ["x"] (Var "x")
  , testCase "App equality" $ App (Var "f") [ILit 1, ILit 2] @?= App (Var "f") [ILit 1, ILit 2]
  , testCase "LetRec equality" $ LetRec "x" (ILit 1) (Var "x") @?= LetRec "x" (ILit 1) (Var "x")
  , testCase "Destruct equality" $ Destruct "a" "b" (Var "p") (Var "body") @?= Destruct "a" "b" (Var "p") (Var "body")
  ]

-- Hedgehog generator for Expr
genExpr :: MonadGen m => Int -> m Expr
genExpr 0 = Gen.choice
  [ ILit <$> Gen.int (Range.constant (-1000) 1000)
  , BLit <$> Gen.bool
  , Var . Text.pack <$> Gen.element ["x","y","z","foo","bar"]
  ]
genExpr n = Gen.recursive Gen.choice
  [ ILit <$> Gen.int (Range.constant (-1000) 1000)
  , BLit <$> Gen.bool
  , Var . Text.pack <$> Gen.element ["x","y","z","foo","bar"]
  ]
  [ BiOp (ArithOp Add) <$> genExpr (n-1) <*> genExpr (n-1)
  , UnOp (ArithUn Neg) <$> genExpr (n-1)
  , Cond <$> genExpr (n-1) <*> genExpr (n-1) <*> genExpr (n-1)
  , LetRec "x" <$> genExpr (n-1) <*> genExpr (n-1)
  , Destruct "a" "b" <$> genExpr (n-1) <*> genExpr (n-1)
  , App <$> genExpr (n-1) <*> Gen.list (Range.linear 0 2) (genExpr (n-1))
  , Lam <$> Gen.list (Range.linear 1 2) (Gen.element ["x","y","z"]) <*> genExpr (n-1)
  ]

prop_expr_show_read_roundtrip :: Property
prop_expr_show_read_roundtrip = property $ do
  e <- forAll (genExpr 3)
  show (read (show e) :: Expr) === show e

exprTests :: TestTree
exprTests = testGroup "Expr"
  [ unit_expr_constructors
  , testProperty "Expr Show/Read roundtrip" prop_expr_show_read_roundtrip
  ]


