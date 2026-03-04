{-# LANGUAGE OverloadedStrings #-}
module Spindle.Parser where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec

import Data.Void
import Control.Monad.Combinators.Expr
import Spindle.Types
import Data.Functor
import Data.Text

type Parser = Parsec Void Text

spaces :: Parser ()
spaces = L.space space1 mempty mempty

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
term = parens (try expr <|> cond <|> letExpr) <|> decimal <?> "term"

identifier :: Parser Text
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar) <&> pack

letExpr :: Parser Expr
letExpr = do
  _ <- symbol "let"
  var <- identifier
  _ <- symbol "="
  val <- expr
  _ <- symbol "in"
  Let var val <$> expr

cond :: Parser Expr
cond = Cond <$> (expr <* symbol "?") <*> (expr <* symbol ":") <*> expr <?> "conditional expression"

table :: [[Operator Parser Expr]]
table = [ [ prefix  "-"  (UnOp Neg)
          , prefix  "+"  id ]
        , [ postfix "++" (UnOp Inc)
          , postfix "--" (UnOp Dec) ]
        , [ binary  "*"  (BiOp Mul)
          , binary  "/"  (BiOp Div) ]
        , [ binary  "+"  (BiOp Add)
          , binary  "-"  (BiOp Sub) ]
        , [ ternary "?" ":" Cond ]
        ]

ternary :: Text -> Text -> (Expr -> Expr -> Expr -> Expr) -> Operator Parser Expr
ternary q c f = TernR ((f <$ symbol c) <$ symbol q)

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixR  (f <$ symbol name)
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)
