{-# LANGUAGE OverloadedStrings #-}
module Spindle.Parser where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec

import Data.Void
import Control.Monad.Combinators.Expr
import Spindle.Types
import Data.Functor
import Data.Text hiding (empty)

type Parser = Parsec Void Text

spaces :: Parser ()
spaces = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol s = L.symbol spaces s <?> unpack s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

decimal :: Parser Expr
decimal = ILit <$> lexeme L.decimal <?> "decimal number"

boolean :: Parser Expr
boolean = (BLit True <$ try (symbol "T"))
      <|> (BLit False <$ try (symbol "F"))
      <?> "boolean literal"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term =
  parens (try expr)
  <|> letTerm
  <|> boolean
  <|> lamTerm
  <|> decimal
  <|> Var <$> identifier
  <?> "term"

lamTerm :: Parser Expr
lamTerm = Lam
    <$> (symbol "\\" *> many identifier)
    <*> (symbol "=>" *> expr)
    <?> "lambda term"

letTerm :: Parser Expr
letTerm = Let
  <$> (symbol "let" *> identifier)
  <*> (symbol ":=" *> expr)
  <*> (symbol "in" *> expr)
  <?> "let term"

identifier :: Parser Text
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar) <&> pack

table :: [[Operator Parser Expr]]
table = [ [ binary "#" toApp ] -- application has the highest precedence
        , [ prefix  "-"  (UnOp (ArithUn Neg))
          ]
        , [ postfix "++" (UnOp (ArithUn Inc))
          , postfix "--" (UnOp (ArithUn Dec))
          ]
        , [ binary  "*"  (BiOp (ArithOp Mul))
          , binary  "/"  (BiOp (ArithOp Div))
          ]
        , [ binary  "+"  (BiOp (ArithOp Add))
          , binary  "-"  (BiOp (ArithOp Sub))
          ]
        , [ binary "==" (BiOp (OrderOp Eq))
          , binary "!=" (BiOp (OrderOp NEq))
          , binary "<=" (BiOp (OrderOp LEq))
          , binary ">=" (BiOp (OrderOp GEq))
          , binary "<"  (BiOp (OrderOp Lt))
          , binary ">"  (BiOp (OrderOp Gt))
          ]
        , [ prefix  "!"  (UnOp (LogicUn Not))  -- logical NOT
          , binary "&&" (BiOp (LogicOp And)) -- logical AND
          , binary "||" (BiOp (LogicOp Or))  -- logical OR
          ]
        , [ ternary "?" ":" Cond ]
        ]

toApp :: Expr -> Expr -> Expr
toApp (App f args) arg = App f (args ++ [arg])
toApp f arg = App f [arg]

ternary :: Text -> Text -> (Expr -> Expr -> Expr -> Expr) -> Operator Parser Expr
ternary q c f = TernR ((f <$ symbol c) <$ symbol q)

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)
