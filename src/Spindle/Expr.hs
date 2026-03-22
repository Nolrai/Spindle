module Spindle.Expr where

import Data.Text
data Expr
  = BLit Bool
  | ILit Int
  | BiOp BiOp Expr Expr
  | UnOp UnOp Expr
  | Cond Expr Expr Expr
  | LetRec Text Expr Expr
  | Destruct Text Text Expr Expr
  | App Expr [Expr]
  | Lam [Text] Expr
  | Var Text
  deriving (Show, Eq, Read)

data ArithBiOp = Add | Sub | Mul | Div
  deriving (Show, Eq, Read)

data LogicBiOp = And | Or
  deriving (Show, Eq, Read)

data OrderBiOp = Eq | NEq | Lt | Gt | LEq | GEq
  deriving (Show, Eq, Read)

data PairOp = Pair
  deriving (Show, Eq, Read)

data UnOp = ArithUn ArithUnOp | LogicUn LogicUnOp
  deriving (Show, Eq, Read)

data ArithUnOp = Neg | Inc | Dec
  deriving (Show, Eq, Read)

data LogicUnOp = Not
  deriving (Show, Eq, Read)

data BiOp = ArithOp ArithBiOp | LogicOp LogicBiOp | OrderOp OrderBiOp | PairOp !PairOp
  deriving (Show, Eq, Read)