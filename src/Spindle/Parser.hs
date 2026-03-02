{-# LANGUAGE OverloadedStrings #-}
module Spindle.Parser where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec

import Data.Void
import Data.Text (Text)
import Control.Monad.Combinators.Expr
type Parser = Parsec Void Text

data Expr
  = Lit Int
  | BiOp BiOp Expr Expr
  | UnOp UnOp Expr
  deriving (Show, Eq)

data BiOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

data UnOp = Neg | Inc | Dec
  deriving (Show, Eq)

spaces :: Parser ()
spaces = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaces

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

decimal :: Parser Expr
decimal = Lit <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens expr <|> decimal <?> "term"

table :: [[Operator Parser Expr]]
table = [ [ prefix  "-"  (UnOp Neg)
          , prefix  "+"  id ]
        , [ postfix "++" (UnOp Inc) ]
        , [ binary  "*"  (BiOp Mul)
          , binary  "/"  (BiOp Div) ]
        , [ binary  "+"  (BiOp Add)
          , binary  "-"  (BiOp Sub) ] ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixR  (f <$ symbol name)
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)