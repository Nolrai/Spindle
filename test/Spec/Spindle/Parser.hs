{-# LANGUAGE OverloadedStrings #-}

module Spec.Spindle.Parser (parserTests, myParser, myParse) where

import Spindle.Parser
import Spindle.Types
import Data.Text (Text)

import Text.Megaparsec
import Control.Monad.Combinators
import Test.Tasty
import Test.Tasty.HUnit

myParser :: Parser Expr
myParser = expr <* eof

myParse :: Parser a -> String -> Text -> Either String a
myParse p name input =
  case parse p name input of
    Left e -> Left (errorBundlePretty e)
    Right result -> Right result

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ constructorTests
  , precedenceTests
  , parseCondTests
  , unnaryTests
  ]

constructorTests :: TestTree
constructorTests = testGroup "constructor tests"
  [ testCase "Parse literal" $
    myParse (decimal <* eof) "42" "42" @?= pure (Lit 42)
  , testCase "Parse Var" $
    myParse myParser "v" "v"
      @?= pure (Var "v")
  , testCase "Parse trivial let" $
    myParse myParser "let v := 2 in v" "let v := 2 in v"
      @?= pure (Let "v" (Lit 2) (Var "v"))
  , testCase "Parse Lambda" $
    myParse myParser "lambda" "fun x => x + 1"
      @?= pure (Lam ["x"] (BiOp Add (Var "x") (Lit 1)))
  , testCase "Parse application" $
    myParse myParser "application" "f 1 2"
      @?= pure (App (Var "f") [Lit 1, Lit 2])
  ]

parseCondTests :: TestTree
parseCondTests = testGroup "conditional operator tests"
  [ testCase "Parse conditional true branch" $
    myParse myParser "(1 ? 2 : 3)" "(1 ? 2 : 3)" @?= pure (Cond (Lit 1) (Lit 2) (Lit 3))
  , testCase "Parse conditional false branch" $
    myParse myParser "(0 ? 2 : 3)" "(0 ? 2 : 3)" @?= pure (Cond (Lit 0) (Lit 2) (Lit 3))
  , testCase "Parse nested conditional" $
    myParse myParser "(1 ? (0 ? 4 : 5) : 3)" "(1 ? (0 ? 4 : 5) : 3)" @?= pure (Cond (Lit 1) (Cond (Lit 0) (Lit 4) (Lit 5)) (Lit 3))
  , testCase "Parse conditional with expression branches" $
    myParse myParser "(1 ? 2 + 3 : 4 * 5)" "(1 ? 2 + 3 : 4 * 5)" @?= pure (Cond (Lit 1) (BiOp Add (Lit 2) (Lit 3)) (BiOp Mul (Lit 4) (Lit 5)))
  ]

unnaryTests :: TestTree
unnaryTests = testGroup "unary operator tests"
  [ testCase "Parse unary minus" $
    myParse myParser "unary minus" "-5" @?= pure (UnOp Neg (Lit 5))
  , testCase "Parse unary plus" $
    myParse myParser "unary plus" "+7" @?= pure (Lit 7)
  , testCase "Parse increment" $
    myParse myParser "increment" "3++" @?= pure (UnOp Inc (Lit 3))
  , testCase "Parse decrement" $
    myParse myParser "decrement" "4--" @?= pure (UnOp Dec (Lit 4))
  , testCase "Parse nested unary and binary" $
    myParse myParser "nested unary and binary" "-(2 + 3)" @?= pure (UnOp Neg (BiOp Add (Lit 2) (Lit 3)))
  , testCase "Parse unary in binary" $
    myParse myParser "unary in binary" "1 + -2" @?= pure (BiOp Add (Lit 1) (UnOp Neg (Lit 2)))
  , testCase "Parse multiple postfix" $
    myParse myParser "multiple postfix" "((2++)--)" @?= pure (UnOp Dec (UnOp Inc (Lit 2)))
  ]

precedenceTests :: TestTree
precedenceTests = testGroup "precedence tests"
  [ testCase "parse precedence 1" $
    myParse myParser "precedence 1" "1 + 2 * 3" @?= pure (BiOp Add (Lit 1) (BiOp Mul (Lit 2) (Lit 3)))
  , testCase "parse precedence 2" $
    myParse myParser "precedence 2" "4 * 5 - 6" @?= pure (BiOp Sub (BiOp Mul (Lit 4) (Lit 5)) (Lit 6))
  , testCase "parse precedence 3" $
    myParse myParser "precedence 3" "8 / 4 + 2" @?= pure (BiOp Add (BiOp Div (Lit 8) (Lit 4)) (Lit 2))
  , testCase "parse precedence 4" $
    myParse myParser "precedence 4" "3 + 4 * 2 - 1" @?= pure (BiOp Sub (BiOp Add (Lit 3) (BiOp Mul (Lit 4) (Lit 2))) (Lit 1))
  , testCase "parse precedence 5" $
    myParse myParser "precedence 5" "6 - 2 * 3 + 4" @?= pure (BiOp Add (BiOp Sub (Lit 6) (BiOp Mul (Lit 2) (Lit 3))) (Lit 4))
  , testCase "parse precedence 6" $
    myParse myParser "precedence 6" "8 / 2 * 3" @?= pure (BiOp Mul (BiOp Div (Lit 8) (Lit 2)) (Lit 3))
  , testCase "parse precedence 7" $
    myParse myParser "precedence 7" "5 + 6 - 7 * 8 / 4" @?= pure (BiOp Sub (BiOp Add (Lit 5) (Lit 6)) (BiOp Div (BiOp Mul (Lit 7) (Lit 8)) (Lit 4)))
  , testCase "parse precedence 8" $
    myParse myParser "precedence 8" "2 * 3 + 4 / 2 - 1" @?= pure (BiOp Sub (BiOp Add (BiOp Mul (Lit 2) (Lit 3)) (BiOp Div (Lit 4) (Lit 2))) (Lit 1))
  , testCase "parse precedence" $
    myParse myParser "precedence" "2 + 3 * 4" @?= pure (BiOp Add (Lit 2) (BiOp Mul (Lit 3) (Lit 4)))
  , testCase "parse precedence with parens" $
    myParse myParser "precedence with parens" "(2 + 3) * 4" @?= pure (BiOp Mul (BiOp Add (Lit 2) (Lit 3)) (Lit 4))
  , testCase "parse division and subtraction" $
    myParse myParser "division and subtraction" "8 / 2 - 1" @?= pure (BiOp Sub (BiOp Div (Lit 8) (Lit 2)) (Lit 1))
  , testCase "parse complex nesting" $
    myParse myParser "complex nesting" "1 + (2 * (3 + 4))" @?= pure (BiOp Add (Lit 1) (BiOp Mul (Lit 2) (BiOp Add (Lit 3) (Lit 4))))
  , testCase "parse chained operations" $
    myParse myParser "chained operations" "1 + 2 + 3 + 4" @?= pure (BiOp Add (BiOp Add (BiOp Add (Lit 1) (Lit 2)) (Lit 3)) (Lit 4))
  ]