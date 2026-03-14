module Spindle.Eval.Common where

import Spindle.Expr
import Control.Monad.RWS
import Control.Monad.Except (MonadError, throwError)
import Data.Text as Text hiding (show)
import Data.Map as Map
import Data.Set
import Control.Monad (void)

-- | Values is the result of evaluating an expression. It can be a literal, or a lambda. In future versions, it will also include closures for functions.
data Thunk
  = NBLit Bool
  | NILit Int
  | NClosure (Map Text Thunk) [Text] Expr
  deriving (Eq)

instance Show Thunk where
  show :: Thunk -> String
  show (NBLit b) = Prelude.show b
  show (NILit n) = Prelude.show n
  show (NClosure env params expr) =
    "Closure " <> show (Map.toList $ void env)
      <> " " <> show params
      <> " (" <> show expr <> ")"

type Env = Map Text Thunk

-- | Tests if a 'thunk' is a int literal, if possible. If not, it throws an error.
matchToILit :: Eval m => Thunk -> m Int
matchToILit (NILit n) = return n
matchToILit e = throwError $ NotILit e

-- | Tests if a 'thunk' is a bool literal, if possible. If not, it throws an error.
matchToBLit :: Eval m => Thunk -> m Bool
matchToBLit (NBLit b) = return b
matchToBLit e = throwError $ NotBLit e

matchClosure :: Eval m => Thunk -> m (Env, [Text], Expr )
matchClosure (NClosure env params body) = return (env, params, body)
matchClosure e = throwError $ NotLambda e

-- | The error type for evaluation. It includes:
-- - `FunNotFound` for when a function is called that doesn't exist in the environment -- not used in this version, but will be needed for the next one when we add functions
-- - `VarNotFound` for when a variable is referenced that doesn't exist in the environment
-- - `Stall` for when evaluation gets stuck on an expression that can't be reduced further
data Err =
  FunNotFound Text (Set Text)
  | VarNotFound Text (Set Text)
  | Stall Expr
  | NotLambda Thunk
  | NotILit Thunk
  | NotBLit Thunk
  | NotLit Thunk
  deriving (Show, Eq)

-- | Log is a list of Text messages that describe the evaluation process.
type Log = [Text]

-- | Eval is a constraint alias for a monad that combines Reader for the environment, Writer for logging, and Except for error handling.
type Eval m = (MonadReader (Map Text Thunk) m, MonadWriter Log m, MonadError Err m)
