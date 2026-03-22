module Main where

import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Spindle.Parser (expr)
import Spindle.HM (inferHMType)
import Spindle.Eval.ByName (eval)
import Text.Megaparsec (parse, errorBundlePretty)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> T.getContents
    (file:_) -> T.readFile file
  case parse expr "<stdin>" input of
    Left err -> do
      putStrLn $ "Parse error: " ++ errorBundlePretty err
      exitFailure
    Right ast -> do
      putStrLn $ "Parsed AST: " ++ show ast
      case inferHMType ast of
        Left errs -> do
          putStrLn $ "Type error: " ++ show errs
          exitFailure
        Right ty -> do
          putStrLn $ "Type: " ++ show ty
          putStrLn $ "Result: " ++ show (eval ast)
