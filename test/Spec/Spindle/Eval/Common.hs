{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Eval.Common (commonTests, testThunkEq) where

-- Placeholder for any tests that are specific to Spindle.Eval.Common
-- Add tests here if/when you have logic in src/Spindle/Eval/Common.hs that needs direct testing.

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map as Map
import Data.List as List
import Spindle.Eval.Common
import Control.Monad (zipWithM_, void)
import GHC.Stack

commonTests :: TestTree
commonTests = testGroup "Common evaluator Tests" []

testThunkEq :: Thunk -> Thunk -> IO ()
testThunkEq
  (NClosure expectedMap expectedVars expectedExpr)
  (NClosure actualMap actualVars actualExpr) = withFrozenCallStack $ do
      expectedExpr @=? actualExpr
      expectedVars @=? actualVars
      void expectedMap @=? void actualMap
      zipWithM_ f (Map.toAscList expectedMap) (Map.toAscList actualMap)
  where
    f (keyL, valL) (keyR, valR) = do
      keyL @=? keyR
      testThunkEq valL valR

testThunkEq expected actual = withFrozenCallStack $ expected @=? actual