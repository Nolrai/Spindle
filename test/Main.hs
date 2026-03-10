{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Spindle.Parser (parserTests)
import Spec.Spindle.Types (typesTests)
import Spec.Spindle.Eval (evaluaterTests)
import Spec.Spindle.HM (hmTests)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testCase "Example test" $
      1 + 1 @?= (2 :: Int)
  , typesTests
  , parserTests
  , evaluaterTests
  , hmTests
  ]