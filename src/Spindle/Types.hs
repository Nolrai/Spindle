module Spindle.Types where

import Data.Text

data Expr
  = BLit Bool
  | ILit Int
  | BiOp BiOp Expr Expr
  | UnOp UnOp Expr
  | Cond Expr Expr Expr
  | Let Text Expr Expr
  | App Expr [Expr]
  | Lam [Text] Expr
  | Var Text
  deriving (Show, Eq)

data ArithBiOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

data LogicBiOp = And | Or
  deriving (Show, Eq)

data OrderBiOp = Eq | NEq | Lt | Gt | LEq | GEq
  deriving (Show, Eq)

data UnOp = ArithUn ArithUnOp | LogicUn LogicUnOp
  deriving (Show, Eq)

data ArithUnOp = Neg | Inc | Dec
  deriving (Show, Eq)

data LogicUnOp = Not
  deriving (Show, Eq)

data BiOp = ArithOp ArithBiOp | LogicOp LogicBiOp | OrderOp OrderBiOp
  deriving (Show, Eq)