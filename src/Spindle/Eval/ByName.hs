{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}
module Spindle.Eval.ByName where

import Spindle.Expr
import Control.Monad.RWS
import Control.Monad.Except (throwError, runExceptT, MonadError (..))
import Data.Text as Text
import Data.Map as Map
import Control.Monad.Writer
import Control.Monad.Reader (runReader)
import Data.List as List
import Spindle.Eval.Common
import Control.Monad (when)

maxEnvSize :: Int
maxEnvSize = 100

matchToLit' :: Eval m => Thunk -> m (Either Int Bool)
matchToLit' (NILit n) = return $ Left n
matchToLit' (NBLit b) = return $ Right b
matchToLit' (NClosure env [] body) = do
  tell ["Evaluating closure for Lit: " <> Text.show body]
  local (const env) (toNormal body) >>= matchToLit'
matchToLit' e = throwError $ NotLit e

matchToILit' :: Eval m => Thunk -> m Int
matchToILit' (NILit n) = return n
matchToILit' (NClosure env [] body) = do
  tell ["Evaluating closure for ILit: " <> Text.show body]
  local (const env) (toNormal body) >>= matchToILit'
matchToILit' e = throwError $ NotILit e

matchToBLit' :: Eval m => Thunk -> m Bool
matchToBLit' (NBLit b) = return b
matchToBLit' (NClosure env [] body) = do
  tell ["Evaluating closure for BLit: " <> Text.show body]
  local (const env) (toNormal body) >>= matchToBLit'
matchToBLit' e = throwError $ NotBLit e

matchClosure' :: Eval m => Thunk -> m (Env, [Text], Expr )
matchClosure' (NClosure env [] body) = do
  tell ["Evaluating closure for Lam: " <> Text.show body]
  local (const env) (toNormal body) >>= matchClosure'
matchClosure' (NClosure env params body) = return (env, params, body)
matchClosure' e = throwError $ NotLambda e

testEnvForSize :: Eval m => m ()
testEnvForSize = do
  env <- ask
  when (Map.size env > maxEnvSize) (throwError $ Stall (ILit (-1)) )

-- | Binds a variable to a value in the environment, but the value is wrapped in a closure that captures the current environment. This allows for recursive definitions, as the variable can refer to itself within its own definition.
bindVarRec :: Eval m => Text -> Expr -> m a -> m a
bindVarRec var val action= do
  env <- ask
  let
    env' = Map.insert var val' env
    val' = NClosure env' [] val
  tell ["Binding variable: " <> var <> " = " <> Text.show val']

  local (const env') (testEnvForSize >> action)

-- | Binds function arguments to their values in the environment, but the values are wrapped in closures that capture the current environment.
bindArgs :: Eval m => Env -> [(Text, Expr)] -> m a -> m a
bindArgs calleeEnv bindings action =
  local (\ callerEnv ->
    let bindings' = NClosure callerEnv [] <$> Map.fromList bindings
      in bindings' `Map.union` calleeEnv)
    (testEnvForSize >> action)

-- | Evaluates an expression to normal form, which is a literal, or a Lambda
-- this is "call by name" evaluation, as opposed to "call by value" which would evaluate the arguments before applying the function. In call by name, we only evaluate as much as needed, which allows for more flexible evaluation and can support features like infinite data structures.

toNormal :: Eval m => Expr -> m Thunk
toNormal focus = do
  tell ["Evaluating: " <> Text.show focus]
  case focus of
    -- For a lambda, we capture the current environment and return a closure. This allows us to support first-class functions and closures.
    Lam names body -> asks $ \env -> NClosure env names body

    -- For a literal, we just return it as a normal form.
    ILit n -> return (NILit n)
    BLit b -> return (NBLit b)

    -- For an application, we first evaluate the function then apply the function to the arguments. See `evalApp` for the application logic.
    App fun args -> do
      fun' <- toNormal fun
      evalApp fun' args

    -- For a variable, we look it up in the environment. If it's not found, we throw an error. If it is found, we return the value.
    Var v -> do
      tell ["Looking up variable: " <> v]
      env <- ask
      case Map.lookup v env of
        Nothing -> throwError $ VarNotFound v (Map.keysSet env)
        Just e -> return e

    -- For a binary operation, we first evaluate the operands to normal form, and then apply the operation. If the operands are not literals, we throw an error.
    BiOp (ArithOp op) e1 e2 -> do
      tell ["Evaluating arithmetic operation: " <> Text.show op]
      e1' <- matchToILit' =<< toNormal e1
      e2' <- matchToILit' =<< toNormal e2
      pure . NILit $ runArithOp op e1' e2'

    BiOp (LogicOp op) e1 e2 -> do
      tell ["Evaluating boolean operation: " <> Text.show op]
      e1' <- matchToBLit' =<< toNormal e1
      e2' <- matchToBLit' =<< toNormal e2
      return $ NBLit (runLogicOp op e1' e2')

    BiOp (OrderOp op) e1 e2 -> do
      tell ["Evaluating order operation: " <> Text.show op]
      e1' <- toNormal e1
      e2' <- toNormal e2
      NBLit <$> do
        result <- matchToLit' e1'
        case result of
          Left n1 -> do
            n2 <- matchToILit' e2'
            return $ runOrdOp op n1 n2
          Right b1 -> do
            b2 <- matchToBLit' e2'
            return $ runOrdOp op b1 b2

    -- For a unary operation, we first evaluate the operand to normal form, and then apply the operation. If the operand is not a literal, we throw an error.
    UnOp (ArithUn op) e -> do
      e' <- matchToILit' =<< toNormal e
      return $ NILit (runArithUn op e')

    UnOp (LogicUn op) e -> do
      tell ["Evaluating unary operation: " <> Text.show op]
      e' <- matchToBLit' =<< toNormal e
      return $ NBLit (runLogicUn op e')

    -- For a conditional, we first evaluate the condition to normal form, and then check if its a boolean literal, if it is evaluate the coresponding expr
    Cond c t f -> do
      tell ["Evaluating conditional expression"]
      c' <- matchToBLit' =<< toNormal c
      if c' then toNormal t else toNormal f

    -- For a let(rec) expression, bind the variable to a closure.
    Let var val body -> bindVarRec var val (toNormal body)
  where
    runLogicOp And = (&&)
    runLogicOp Or = (||)

    runLogicUn Not = not

    runOrdOp Eq = (==)
    runOrdOp NEq = (/=)
    runOrdOp Lt = (<)
    runOrdOp Gt = (>)
    runOrdOp LEq = (<=)
    runOrdOp GEq = (>=)

    runArithOp Add = (+)
    runArithOp Sub = (-)
    runArithOp Mul = (*)
    runArithOp Div = \ x y -> if y == 0 then 0 else x `div` y

    runArithUn Neg = negate
    runArithUn Inc = (+ 1)
    runArithUn Dec = \ x -> x - 1

evalApp :: Eval m => Thunk -> [Expr] -> m Thunk
evalApp fun args = do
  (calleeEnv, params, body) <- matchClosure' fun
  let matchedParams   = List.zip params args
      used            = List.length matchedParams
      remainingParams = List.drop used params
      remainingArgs   = List.drop used args

  let cont =
        case (remainingParams, remainingArgs) of
          (_:_, []) -> (\env -> NClosure env remainingParams body) <$> ask

          ([], []) -> toNormal body

          ([], _ : _) -> do
            fun' <- toNormal body
            evalApp fun' remainingArgs

          (_:_, _:_ ) -> error "Impossible"

  bindArgs calleeEnv matchedParams cont

-- | A helper function to evaluate an expression in an empty environment and get either the result or the error.
eval :: Expr -> Either Err Thunk
eval = fst . runEval Map.empty

-- These three functions each strip away one layer of the Eval monad stack. `runEval` runs the entire Eval monad stack, `getLogs` runs the Writer layer to get the logs, and `getErrors` runs the Except layer to get either the result or the error.

runEval :: Map Text Thunk -> Expr -> (Either Err Thunk, Log)
runEval env e = runReader (getLogs e) env

getLogs :: (MonadReader (Map Text Thunk) m) => Expr -> m (Either Err Thunk, Log)
getLogs e = runWriterT (getErrors e)

getErrors :: (MonadReader (Map Text Thunk) m, MonadWriter Log m) =>
  Expr -> m (Either Err Thunk)
getErrors e = runExceptT $ force e

-- | This function evaluates an expression to normal form, but it also handles the case where the expression is a zero argument closure that can be evaluated to a literal. This is needed for properly evaluating simple expresions, because otherwise we would end up with a closure that contains the expression instead of the literal result of the expression.
force :: Eval m => Expr -> m Thunk
force e = toNormal e >>= force'

-- | This function takes a thunk that is the result of evaluating an expression, and if it's a zero argument closure, it evaluates the body of the closure to normal form and returns the result. This allows us to properly evaluate simple expressions that would otherwise be wrapped in a closure.
force' :: Eval m => Thunk -> m Thunk
force' thunk = do
  let tryClosure = do
        (env, vars, body) <- matchClosure' thunk;
        pure $ NClosure env vars body
  (either NILit NBLit <$> matchToLit' thunk)
    `catchError` \case
      NotLit _ -> tryClosure
      NotBLit _ -> tryClosure
      NotILit _ -> tryClosure
      err -> throwError err

