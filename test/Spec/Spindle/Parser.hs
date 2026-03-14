{-# LANGUAGE OverloadedStrings #-}

module Spec.Spindle.Parser (parserTests, myParser) where

import Spindle.Parser
import Spindle.Expr
import Utils

import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft)

myParser :: Parser Expr
myParser = expr <* eof

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ invalidParserTests
  , constructorTests
  , precedenceTests
  , parseCondTests
  , boolExprParserTests
  , unnaryTests
  , lamAppTests
  ]

-- | Tests for boolean expressions and operators in the parser
boolExprParserTests :: TestTree
boolExprParserTests = testGroup "boolean expression parser tests"
  [ testCase "Parse bool literal T" $
    myParse myParser "T" "T" ?= BLit True
  , testCase "Parse bool literal F" $
    myParse myParser "F" "F" ?= BLit False
  , testCase "Parse and T T" $
    myParse myParser "T && T" "T && T" ?= BiOp (LogicOp And) (BLit True) (BLit True)
  , testCase "Parse and T F" $
    myParse myParser "T && F" "T && F" ?= BiOp (LogicOp And) (BLit True) (BLit False)
  , testCase "Parse or F F" $
    myParse myParser "F || F" "F || F" ?= BiOp (LogicOp Or) (BLit False) (BLit False)
  , testCase "Parse or T F" $
    myParse myParser "T || F" "T || F" ?= BiOp (LogicOp Or) (BLit True) (BLit False)
  , testCase "Parse not T" $
    myParse myParser "!T" "!T" ?= UnOp (LogicUn Not) (BLit True)
  , testCase "Parse not F" $
    myParse myParser "!F" "!F" ?= UnOp (LogicUn Not) (BLit False)
  , testCase "Parse equality T" $
    myParse myParser "T == T" "T == T" ?= BiOp (OrderOp Eq) (BLit True) (BLit True)
  , testCase "Parse equality F" $
    myParse myParser "T == F" "T == F" ?= BiOp (OrderOp Eq) (BLit True) (BLit False)
  , testCase "Parse inequality T" $
    myParse myParser "T != F" "T != F" ?= BiOp (OrderOp NEq) (BLit True) (BLit False)
  , testCase "Parse inequality F" $
    myParse myParser "F != F" "F != F" ?= BiOp (OrderOp NEq) (BLit False) (BLit False)
  , testCase "Parse less than T" $
    myParse myParser "1 < 2" "1 < 2" ?= BiOp (OrderOp Lt) (ILit 1) (ILit 2)
  , testCase "Parse less than F" $
    myParse myParser "2 < 1" "2 < 1" ?= BiOp (OrderOp Lt) (ILit 2) (ILit 1)
  , testCase "Parse greater than T" $
    myParse myParser "3 > 2" "3 > 2" ?= BiOp (OrderOp Gt) (ILit 3) (ILit 2)
  , testCase "Parse greater than F" $
    myParse myParser "2 > 3" "2 > 3" ?= BiOp (OrderOp Gt) (ILit 2) (ILit 3)
  , testCase "Parse less or equal T" $
    myParse myParser "2 <= 2" "2 <= 2" ?= BiOp (OrderOp LEq) (ILit 2) (ILit 2)
  , testCase "Parse less or equal F" $
    myParse myParser "3 <= 2" "3 <= 2" ?= BiOp (OrderOp LEq) (ILit 3) (ILit 2)
  , testCase "Parse greater or equal T" $
    myParse myParser "3 >= 2" "3 >= 2" ?= BiOp (OrderOp GEq) (ILit 3) (ILit 2)
  , testCase "Parse greater or equal F" $
    myParse myParser "1 >= 2" "1 >= 2" ?= BiOp (OrderOp GEq) (ILit 1) (ILit 2)
  , testCase "Parse complex boolean expr" $
    myParse myParser "(1 < 2) && (3 > 2) || F" "(1 < 2) && (3 > 2) || F" ?= BiOp (LogicOp Or) (BiOp (LogicOp And) (BiOp (OrderOp Lt) (ILit 1) (ILit 2)) (BiOp (OrderOp Gt) (ILit 3) (ILit 2))) (BLit False)
  ]

invalidParserTests :: TestTree
invalidParserTests = testGroup "Invalid Parser Tests"
  [ testCase "Parse invalid let" $
    assertBool "Should fail to parse invalid let expression" $
      isLeft (parse (myParser <* eof) "" "let x 5 in x")
  , testCase "Parse invalid lambda" $
    assertBool "Should fail to parse invalid lambda expression" $
      isLeft (parse (myParser <* eof) "" "\\ x x + 1")
  , testCase "Parse invalid application" $
    assertBool "Should fail to parse invalid application expression" $
      isLeft (parse (myParser <* eof) "" "f # 1 #")
  , testCase "Parse invalid conditional" $
    assertBool "Should fail to parse invalid conditional expression" $
      isLeft (parse (myParser <* eof) "" "(1 ? 2 :)")
  , testCase "Parse invalid unary operator" $
    assertBool "Should fail to parse invalid unary operator expression" $
      isLeft (parse (myParser <* eof) "" "-")
  , testCase "Parse invalid binary operator" $
    assertBool "Should fail to parse invalid binary operator expression" $
      isLeft (parse (myParser <* eof) "" "1 +")
  ]

constructorTests :: TestTree
constructorTests = testGroup "constructor tests"
  [ testCase "Parse int literal" $
    myParse (decimal <* eof) "42" "42" ?= ILit 42
  , testCase "Parse F literal" $
    myParse (boolean <* eof) "F" "F" ?= BLit False
  , testCase "Parse T literal" $
    myParse (boolean <* eof) "T" "T" ?= BLit True
  , testCase "Parse Var" $
    myParse myParser "v" "v"
      ?= Var "v"
  , testCase "Parse trivial let" $
    myParse myParser "let v := 2 in v" "let v := 2 in v"
      ?= Let "v" (ILit 2) (Var "v")
  , testCase "Parse Lambda" $
    myParse myParser "lambda" "\\ x => x + 1"
      ?= Lam ["x"] (BiOp (ArithOp Add) (Var "x") (ILit 1))
  , testCase "Parse application" $
    myParse myParser "application" "f # 1 # 2"
      ?= App (Var "f") [ILit 1, ILit 2]
  ]

parseCondTests :: TestTree
parseCondTests = testGroup "conditional operator tests"
  [ testCase "Parse conditional T branch" $
    myParse myParser "(T ? 2 : 3)" "(T ? 2 : 3)" ?= Cond (BLit True) (ILit 2) (ILit 3)
  , testCase "Parse conditional F branch" $
    myParse myParser "(F ? 2 : 3)" "(F ? 2 : 3)" ?= Cond (BLit False) (ILit 2) (ILit 3)
  , testCase "Parse nested conditional" $
    myParse myParser "(T ? (F ? 4 : 5) : 3)" "(T ? (F ? 4 : 5) : 3)" ?= Cond (BLit True) (Cond (BLit False) (ILit 4) (ILit 5)) (ILit 3)
  , testCase "Parse conditional with expression branches" $
    myParse myParser "(T ? 2 + 3 : 4 * 5)" "(T ? 2 + 3 : 4 * 5)" ?= Cond (BLit True) (BiOp (ArithOp Add) (ILit 2) (ILit 3)) (BiOp (ArithOp Mul) (ILit 4) (ILit 5))
  ]

unnaryTests :: TestTree
unnaryTests = testGroup "unary operator tests"
  [ testCase "Parse unary minus" $
    myParse myParser "unary minus" "-5" ?= UnOp (ArithUn Neg) (ILit 5)
  , testCase "Parse increment" $
    myParse myParser "increment" "3++" ?= UnOp (ArithUn Inc) (ILit 3)
  , testCase "Parse decrement" $
    myParse myParser "decrement" "4--" ?= UnOp (ArithUn Dec) (ILit 4)
  , testCase "Parse nested unary and binary" $
    myParse myParser "nested unary and binary" "-(2 + 3)" ?= UnOp (ArithUn Neg) (BiOp (ArithOp Add) (ILit 2) (ILit 3))
  , testCase "Parse unary in binary" $
    myParse myParser "unary in binary" "1 + -2" ?= BiOp (ArithOp Add) (ILit 1) (UnOp (ArithUn Neg) (ILit 2))
  , testCase "Parse multiple postfix" $
    myParse myParser "multiple postfix" "((2++)--)" ?= UnOp (ArithUn Dec) (UnOp (ArithUn Inc) (ILit 2))
  ]

precedenceTests :: TestTree
precedenceTests = testGroup "precedence tests"
  [ testCase "parse precedence 1" $
    myParse myParser "precedence 1" "1 + 2 * 3" ?= BiOp (ArithOp Add) (ILit 1) (BiOp (ArithOp Mul) (ILit 2) (ILit 3))
  , testCase "parse precedence 2" $
    myParse myParser "precedence 2" "4 * 5 - 6" ?= BiOp (ArithOp Sub) (BiOp (ArithOp Mul) (ILit 4) (ILit 5)) (ILit 6)
  , testCase "parse precedence 3" $
    myParse myParser "precedence 3" "8 / 4 + 2" ?= BiOp (ArithOp Add) (BiOp (ArithOp Div) (ILit 8) (ILit 4)) (ILit 2)
  , testCase "parse precedence 4" $
    myParse myParser "precedence 4" "3 + 4 * 2 - 1" ?= BiOp (ArithOp Sub) (BiOp (ArithOp Add) (ILit 3) (BiOp (ArithOp Mul) (ILit 4) (ILit 2))) (ILit 1)
  , testCase "parse precedence 5" $
    myParse myParser "precedence 5" "6 - 2 * 3 + 4" ?= BiOp (ArithOp Add) (BiOp (ArithOp Sub) (ILit 6) (BiOp (ArithOp Mul) (ILit 2) (ILit 3))) (ILit 4)
  , testCase "parse precedence 6" $
    myParse myParser "precedence 6" "8 / 2 * 3" ?= BiOp (ArithOp Mul) (BiOp (ArithOp Div) (ILit 8) (ILit 2)) (ILit 3)
  , testCase "parse precedence 7" $
    myParse myParser "precedence 7" "5 + 6 - 7 * 8 / 4" ?= BiOp (ArithOp Sub) (BiOp (ArithOp Add) (ILit 5) (ILit 6)) (BiOp (ArithOp Div) (BiOp (ArithOp Mul) (ILit 7) (ILit 8)) (ILit 4))
  , testCase "parse precedence 8" $
    myParse myParser "precedence 8" "2 * 3 + 4 / 2 - 1" ?= BiOp (ArithOp Sub) (BiOp (ArithOp Add) (BiOp (ArithOp Mul) (ILit 2) (ILit 3)) (BiOp (ArithOp Div) (ILit 4) (ILit 2))) (ILit 1)
  , testCase "parse precedence" $
    myParse myParser "precedence" "2 + 3 * 4" ?= BiOp (ArithOp Add) (ILit 2) (BiOp (ArithOp Mul) (ILit 3) (ILit 4))
  , testCase "parse precedence with parens" $
    myParse myParser "precedence with parens" "(2 + 3) * 4" ?= BiOp (ArithOp Mul) (BiOp (ArithOp Add) (ILit 2) (ILit 3)) (ILit 4)
  , testCase "parse division and subtraction" $
    myParse myParser "division and subtraction" "8 / 2 - 1" ?= BiOp (ArithOp Sub) (BiOp (ArithOp Div) (ILit 8) (ILit 2)) (ILit 1)
  , testCase "parse complex nesting" $
    myParse myParser "complex nesting" "1 + (2 * (3 + 4))" ?= BiOp (ArithOp Add) (ILit 1) (BiOp (ArithOp Mul) (ILit 2) (BiOp (ArithOp Add) (ILit 3) (ILit 4)))
  , testCase "parse chained operations" $
    myParse myParser "chained operations" "1 + 2 + 3 + 4" ?= BiOp (ArithOp Add) (BiOp (ArithOp Add) (BiOp (ArithOp Add) (ILit 1) (ILit 2)) (ILit 3)) (ILit 4)
  ]

lamAppTests :: TestTree
lamAppTests = testGroup "lambda application tests"
  [ testCase "parse simple lambda application" $
    myParse myParser "simple lambda application" "(\\ x => x + 1) # 5" ?=  App (Lam ["x"] (BiOp (ArithOp Add) (Var "x") (ILit 1))) [ILit 5]
  , testCase "parse lambda application with multiple parameters" $
    myParse myParser "lambda application with multiple parameters" "(\\ x y => x * y) # 3 # 4" ?= App (Lam ["x", "y"] (BiOp (ArithOp Mul) (Var "x") (Var "y"))) [ILit 3, ILit 4]
  , testCase "parse lambda application with nested lambdas" $
    myParse myParser "lambda application with nested lambdas" "(  \\ x => \\ y => x + y) # 2 # 3" ?= App (Lam ["x"] (Lam ["y"] (BiOp (ArithOp Add) (Var "x") (Var "y")))) [ILit 2, ILit 3]
  , testCase "parse lambda application with excess arguments" $
    myParse myParser "lambda application with excess arguments" "(\\ x => x + 1) # 5 # 6" ?= App (Lam ["x"] (BiOp (ArithOp Add) (Var "x") (ILit 1))) [ILit 5, ILit 6]
  ]