module Utils where

import Text.Megaparsec
import Spindle.Parser
import Data.Text as Text
import Test.Tasty.HUnit

myParse :: Parser a -> String -> Text -> Either String a
myParse p name input =
  case parse p name input of
    Left e -> Left (errorBundlePretty e)
    Right result -> Right result

(?=) :: (Eq a, Show a) => Either String a -> a -> IO ()
result ?= expected =
  case result of
    Left err -> assertFailure err
    Right val -> val @?= expected