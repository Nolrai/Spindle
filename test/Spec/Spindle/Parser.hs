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
  [ testCase "Parse literal" $
    myParse (decimal <* eof) "42" "42" @?= pure (Lit 42)
  , testCase "Parse simple expression" $
    myParse myParser "simple expression" "1 + 2" @?= pure (BiOp Add (Lit 1) (Lit 2))
  , testCase "Parse nested expression" $
    parse myParser "nested expression" "1 + 2 + 3" @?= pure (BiOp Add (Lit 1) (BiOp Add (Lit 2) (Lit 3)))
  , testCase "Parse expression with parentheses" $
    parseMaybe myParser "(1 + 2) + 3" @?= pure (BiOp Add (BiOp Add (Lit 1) (Lit 2)) (Lit 3))
  , testCase "Parse unary minus" $
    myParse myParser "-5" "-5" @?= pure (UnOp Neg (Lit 5))
  , testCase "Parse unary plus" $
    myParse myParser "+7" "+7" @?= pure (Lit 7)
  , testCase "Parse increment" $
    myParse myParser "3++" "3++" @?= pure (UnOp Inc (Lit 3))
  , testCase "Parse decrement" $
    myParse myParser "4--" "4--" @?= pure (UnOp Dec (Lit 4))
  , testCase "Parse nested unary and binary" $
    myParse myParser "-(2 + 3)" "-(2 + 3)" @?= pure (UnOp Neg (BiOp Add (Lit 2) (Lit 3)))
  , testCase "Parse unary in binary" $
    myParse myParser "1 + -2" "1 + -2" @?= pure (BiOp Add (Lit 1) (UnOp Neg (Lit 2)))
  , testCase "Parse multiple postfix" $
    myParse myParser "((2++)--)" "((2++)--)" @?= pure (UnOp Dec (UnOp Inc (Lit 2)))
  , testCase "Parse operator precedence" $
    myParse myParser "2 + 3 * 4" "2 + 3 * 4" @?= pure (BiOp Add (Lit 2) (BiOp Mul (Lit 3) (Lit 4)))
  , testCase "Parse precedence with parens" $
    myParse myParser "(2 + 3) * 4" "(2 + 3) * 4" @?= pure (BiOp Mul (BiOp Add (Lit 2) (Lit 3)) (Lit 4))
  , testCase "Parse division and subtraction" $
    myParse myParser "8 / 2 - 1" "8 / 2 - 1" @?= pure (BiOp Sub (BiOp Div (Lit 8) (Lit 2)) (Lit 1))
  , testCase "Parse complex nesting" $
    myParse myParser "1 + (2 * (3 + 4))" "1 + (2 * (3 + 4))" @?= pure (BiOp Add (Lit 1) (BiOp Mul (Lit 2) (BiOp Add (Lit 3) (Lit 4))))
  , testCase "Parse chained operations" $
    myParse myParser "1 + 2 + 3 + 4" "1 + 2 + 3 + 4" @?= pure (BiOp Add (Lit 1) (BiOp Add (Lit 2) (BiOp Add (Lit 3) (Lit 4))))
  , testCase "Parse conditional true branch" $
    myParse myParser "(1 ? 2 : 3)" "(1 ? 2 : 3)" @?= pure (Cond (Lit 1) (Lit 2) (Lit 3))
  , testCase "Parse conditional false branch" $
    myParse myParser "(0 ? 2 : 3)" "(0 ? 2 : 3)" @?= pure (Cond (Lit 0) (Lit 2) (Lit 3))
  , testCase "Parse nested conditional" $
    myParse myParser "(1 ? (0 ? 4 : 5) : 3)" "(1 ? (0 ? 4 : 5) : 3)" @?= pure (Cond (Lit 1) (Cond (Lit 0) (Lit 4) (Lit 5)) (Lit 3))
  , testCase "Parse conditional with expression branches" $
    myParse myParser "(1 ? 2 + 3 : 4 * 5)" "(1 ? 2 + 3 : 4 * 5)" @?= pure (Cond (Lit 1) (BiOp Add (Lit 2) (Lit 3)) (BiOp Mul (Lit 4) (Lit 5)))
  , testCase "Parse Var" $
    myParse myParser "v" "v"
      @?= pure (Var "v")
  , testCase "Parse trivial let" $
    myParse myParser "let v := 2 in v" "let v := 2 in v"
      @?= pure (Let "v" (Lit 2) (Var "v"))
  ]