{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spindle.Parser (expr, Expr(..), Parser, BiOp(..), UnOp(..))
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testCase "Example test" $
      1 + 1 @?= (2 :: Int)
  , parserTests
  ]

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
    myParse myParser "42" "42" @?= pure (Lit 42)
  , testCase "Parse simple expression" $
    myParse myParser "simple expression" "1 + 2" @?= pure (BiOp Add (Lit 1) (Lit 2))
  , testCase "Parse nested expression" $
    parse myParser "nested expression" "1 + 2 + 3" @?= pure (BiOp Add (Lit 1) (BiOp Add (Lit 2) (Lit 3)))
  , testCase "Parse expression with parentheses" $
    parseMaybe myParser "(1 + 2) + 3" @?= pure (BiOp Add (BiOp Add (Lit 1) (Lit 2)) (Lit 3))
  ]