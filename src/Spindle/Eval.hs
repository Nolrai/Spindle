{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}
module Spindle.Eval where

import Spindle.Types
import Control.Monad.RWS
import Control.Monad.Except (MonadError, throwError, runExceptT)
import Data.Text as Text
import Data.Map as Map
import Data.Set
import Control.Monad.Writer
import Control.Monad.Reader (runReader)

-- | The error type for evaluation. It includes:
-- - `FunNotFound` for when a function is called that doesn't exist in the environment -- not used in this version, but will be needed for the next one when we add functions
-- - `VarNotFound` for when a variable is referenced that doesn't exist in the environment
-- - `Stall` for when evaluation gets stuck on an expression that can't be reduced further
data Err =
  FunNotFound Text (Set Text)
  | VarNotFound Text (Set Text)
  | Stall Expr
  deriving (Show, Eq)

-- | Log is a list of Text messages that describe the evaluation process.
type Log = [Text]

-- | Eval is a constraint alias for a monad that combines Reader for the environment, Writer for logging, and Except for error handling.
type Eval m = (MonadReader (Map Text Int) m, MonadWriter Log m, MonadError Err m)

-- | Tests if expression to a literal, if possible. If not, it throws an error.
matchToLit :: Eval m => Expr -> m Int
matchToLit (Lit n) = return n
matchToLit e = throwError $ Stall e

-- | Evaluates an expression to normal form, which is a literal.
-- this is "call by value" evaluation, as opposed to "call by name" which would only evaluate as much as needed.
toNormal :: Eval m => Expr -> m Expr
toNormal focus = do
  tell ["Evaluating: " <> Text.show focus]
  case focus of
    Lit n -> return (Lit n)
    Var v -> do
      tell ["Looking up variable: " <> v]
      env <- ask
      case Map.lookup v env of
        Nothing -> throwError $ VarNotFound v (Map.keysSet env)
        Just e -> return (Lit e)
    BiOp op e1 e2 ->
      tell ["Evaluating binary operation: " <> Text.show op] >>
      let f = case op of
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> (\ x y -> if y == 0 then 0 else x `div` y)
          in do
        e1' <- matchToLit =<< toNormal e1
        e2' <- matchToLit =<< toNormal e2
        return $ Lit (f e1' e2')
    UnOp op e -> do
      tell ["Evaluating unary operation: " <> Text.show op]
      let f = case op of
                Neg -> negate
                Inc -> (+ 1)
                Dec -> (\ x -> if x == 0 then 0 else x - 1)
      e' <- matchToLit =<< toNormal e
      return $ Lit (f e')
    Cond c t f -> do
      tell ["Evaluating conditional expression"]
      c' <- matchToLit =<< toNormal c
      if c' /= 0 then toNormal t else toNormal f
    Let var val body ->
      do
        val' <- matchToLit =<< toNormal val
        tell ["Binding variable: " <> var <> " = " <> Text.show val']
        local (Map.insert var val') (toNormal body)

getErrors :: (MonadReader (Map Text Int) m, MonadWriter Log m) =>
  Expr -> m (Either Err Int)
getErrors e = runExceptT (toNormal e >>= matchToLit)

getLogs :: (MonadReader (Map Text Int) m) => Expr -> m (Either Err Int, Log)
getLogs e = runWriterT (getErrors e)

runEval :: Map Text Int -> Expr -> (Either Err Int, Log)
runEval env e = runReader (getLogs e) env

eval :: Expr -> Either Err Int
eval = fst . runEval mempty