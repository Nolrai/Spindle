module Spindle.Types where

import Data.Text

data Expr
  = Lit Int
  | BiOp BiOp Expr Expr
  | UnOp UnOp Expr
  | Cond Expr Expr Expr
  | Let Text Expr Expr
  | App Expr [Expr]
  | Lam [Text] Expr
  | Var Text
  deriving (Show, Eq)

data BiOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

data UnOp = Neg | Inc | Dec
  deriving (Show, Eq)