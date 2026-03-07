{-# LANGUAGE OverloadedStrings #-}
module Spec.Spindle.Eval (evaluaterTests) where

import Spindle.Eval
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Spindle.Parser (myParser)
import Spindle.Types
import Utils
import Data.Text (Text)
import GHC.IsList (fromList)

evaluaterTests :: TestTree
evaluaterTests = testGroup "evaluator Tests"
  [ simpleConstructorTests
  , precedenceTests
  , evalCondTests
  , letTests
  , lamAppTests
  ]

-- | Helper function to create an eval test case from an input string and expected output
evalTestCase :: String -> Text -> Either Err NormalForm -> TestTree
evalTestCase name input expected =
  testCase name $
    (eval <$> myParse myParser name input) ?= expected

-- | Tests for simple expression constructors, to make sure they are being evaluated correctly
simpleConstructorTests :: TestTree
simpleConstructorTests = testGroup "expr constructor tests"
  [ evalTestCase "literal" "42" (Right (NLit 42))
  , evalTestCase "addition" "1 + 2" (Right (NLit 3))
  , evalTestCase "multiplication" "3 * 4" (Right (NLit 12))
  , evalTestCase "division" "8 / 2" (Right (NLit 4))
  , evalTestCase "division by zero" "8 / 0" (Right (NLit 0))
  , evalTestCase "subtraction" "5 - 2" (Right (NLit 3))
  , evalTestCase "unary minus" "-5" (Right (NLit (-5)))
  , evalTestCase "increment" "3++" (Right (NLit 4))
  , evalTestCase "decrement" "4--" (Right (NLit 3))
  ]

-- | Tests for operator precedence, to make sure that the correct order of operations is being followed
precedenceTests :: TestTree
precedenceTests = testGroup "precedence tests"
  [ evalTestCase "precedence 1" "1 + 2 * 3" (Right (NLit 7))
  , evalTestCase "precedence 2" "4 * 5 - 6" (Right (NLit 14))
  , evalTestCase "precedence 3" "8 / 4 + 2" (Right (NLit 4))
  , evalTestCase "precedence 4" "3 + 4 * 2 - 1" (Right (NLit 10))
  , evalTestCase "precedence 5" "6 - 2 * 3 + 4" (Right (NLit 4))
  , evalTestCase "precedence 6" "8 / 2 * 3" (Right (NLit 12))
  , evalTestCase "precedence 7" "5 + 6 - 7 * 8 / 4" (Right (NLit (-3)))
  , evalTestCase "precedence 8" "2 * 3 + 4 / 2 - 1" (Right (NLit 7))
  , evalTestCase "precedence" "2 + 3 * 4" (Right (NLit 14))
  , evalTestCase "precedence with parens" "(2 + 3) * 4" (Right (NLit 20))
  , evalTestCase "division and subtraction" "8 / 2 - 1" (Right (NLit 3))
  , evalTestCase "complex nesting" "1 + (2 * (3 + 4))" (Right (NLit 15))
  , evalTestCase "chained operations" "1 + 2 + 3 + 4" (Right (NLit 10))
  ]

-- | Tests for conditional expressions, to make sure that the correct branch is being evaluated based on the condition
evalCondTests :: TestTree
evalCondTests = testGroup "conditional tests"
  [ evalTestCase "conditional true branch" "(1 ? 2 : 3)" (Right (NLit 2))
  , evalTestCase "conditional false branch" "(0 ? 2 : 3)" (Right (NLit 3))
  , evalTestCase "nested conditional" "(1 ? (0 ? 4 : 5) : 3)" (Right (NLit 5))
  , evalTestCase "conditional with expression branches" "(1 ? 2 + 3 : 4 * 5)" (Right (NLit 5))
  , evalTestCase "conditional with nested expressions" "(1 ? (2 + 3) * 4 : 5 - 6)" (Right (NLit 20))
  ]

-- | Tests for lambda application, to make sure that closures are being applied correctly and that the correct number of arguments are being passed
lamAppTests :: TestTree
lamAppTests = testGroup "lambda application tests"
  [ evalTestCase "simple lambda application" "(\\ x => x + 1) # 5" (Right (NLit 6))
  , evalTestCase "lambda application with multiple parameters" "(\\ x y => x * y) # 3 # 4" (Right (NLit 12))
  , evalTestCase "lambda application with nested lambdas" "(  \\ x => \\ y => x + y) # 2 # 3" (Right (NLit 5))
  , evalTestCase "lambda application with excess arguments" "(\\ x => x + 1) # 3 # 6" (Left (NotLambda (NLit 4)))
  , evalTestCase "lambda application with excess parameters" "(\\ x y => x + y) # 2" (Right (NClosure (fromList [("x", NLit 2)]) ["y"] (BiOp Add (Var "x") (Var "y"))))
  , evalTestCase "closure captures defining environment" alphaRenamingText1 (Right (NLit 42))
  , evalTestCase "inner binder does not capture closure variable" alphaRenamingText2 (Right (NLit 42))
  , evalTestCase "later shadowing does not affect closure variable" alphaRenamingText3 (Right (NLit 5))
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

-- | Tests for let bindings, to make sure that variables are being bound correctly and that shadowing is working as expected
letTests :: TestTree
letTests = testGroup "let binding tests"
  [ evalTestCase "eval let binding" "let v := 2 in v" (Right (NLit 2))
  , evalTestCase "eval let binding with expression" "let x := 1 + 2 in x + 3" (Right (NLit 6))
  , evalTestCase "eval nested let bindings" "let x := 2 in let y := x + 3 in y * 2" (Right (NLit 10))
  , evalTestCase "eval let shadowing" "let x := 1 in let x := 2 in x + x" (Right (NLit 4))
  ]
