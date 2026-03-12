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
import Data.List as List

-- | Values is the result of evaluating an expression. It can be a literal, or a lambda. In future versions, it will also include closures for functions.
data NormalForm
  = NBLit Bool
  | NILit Int
  | NClosure (Map Text NormalForm) [Text] Expr
  deriving (Show, Eq)

type Env = Map Text NormalForm

-- | The error type for evaluation. It includes:
-- - `FunNotFound` for when a function is called that doesn't exist in the environment -- not used in this version, but will be needed for the next one when we add functions
-- - `VarNotFound` for when a variable is referenced that doesn't exist in the environment
-- - `Stall` for when evaluation gets stuck on an expression that can't be reduced further
data Err =
  FunNotFound Text (Set Text)
  | VarNotFound Text (Set Text)
  | Stall Expr
  | NotLambda NormalForm
  | NotILit NormalForm
  | NotBLit NormalForm
  | NotLit NormalForm
  deriving (Show, Eq)

-- | Log is a list of Text messages that describe the evaluation process.
type Log = [Text]

-- | Eval is a constraint alias for a monad that combines Reader for the environment, Writer for logging, and Except for error handling.
type Eval m = (MonadReader (Map Text NormalForm) m, MonadWriter Log m, MonadError Err m)

-- | Tests if expression to a literal, if possible. If not, it throws an error.
matchToILit :: Eval m => NormalForm -> m Int
matchToILit (NILit n) = return n
matchToILit e = throwError $ NotILit e

-- | Tests if expression to a literal, if possible. If not, it throws an error.
matchToBLit :: Eval m => NormalForm -> m Bool
matchToBLit (NBLit b) = return b
matchToBLit e = throwError $ NotBLit e

matchClosure :: Eval m => NormalForm -> m (Env, [Text], Expr )
matchClosure (NClosure env params body) = return (env, params, body)
matchClosure e = throwError $ NotLambda e

-- | Evaluates an expression to normal form, which is a literal, or a Lambda
-- this is "call by value" evaluation, as opposed to "call by name" which would only evaluate as much as needed.
toNormal :: Eval m => Expr -> m NormalForm
toNormal focus = do
  tell ["Evaluating: " <> Text.show focus]
  case focus of
    -- For a lambda, we capture the current environment and return a closure. This allows us to support first-class functions and closures.
    Lam names body -> asks $ \env -> NClosure env names body

    -- For a literal, we just return it as a normal form.
    ILit n -> return (NILit n)
    BLit b -> return (NBLit b)

    -- For an application, we first evaluate the function and the arguments to normal form, and then apply the function to the arguments. See `evalApp` for the application logic.
    App fun args -> do
      funVal <- toNormal fun
      argValues <- mapM toNormal args
      evalApp funVal argValues

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
      let f = case op of
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> (\ x y -> if y == 0 then 0 else x `div` y)
          in do
        e1' <- matchToILit =<< toNormal e1
        e2' <- matchToILit =<< toNormal e2
        pure . NILit $ f e1' e2'

    BiOp (LogicOp op) e1 e2 -> do
      tell ["Evaluating boolean operation: " <> Text.show op]
      let f = case op of
                And -> (&&)
                Or -> (||)
          in do
        e1' <- matchToBLit =<< toNormal e1
        e2' <- matchToBLit =<< toNormal e2
        return $ NBLit (f e1' e2')

    BiOp (OrderOp op) e1 e2 -> do
      tell ["Evaluating order operation: " <> Text.show op]
      let f = case op of
                Eq -> (==)
                NEq -> (/=)
                Lt -> (<)
                Gt -> (>)
                LEq -> (<=)
                GEq -> (>=)
          in do
        e1' <- toNormal e1
        e2' <- toNormal e2
        NBLit <$>
          case (e1', e2') of
            (NILit n1, NILit n2) -> pure $ f n1 n2
            (NBLit b1, NBLit b2) ->
              let toInt b = if b then 1 else 0
                in pure $ f (toInt b1) (toInt b2)
            (NILit _, _) -> throwError $ NotBLit e2'
            (NBLit _, _) -> throwError $ NotILit e2'
            (NClosure {}, _) -> throwError $ NotLit e2'

    -- For a unary operation, we first evaluate the operand to normal form, and then apply the operation. If the operand is not a literal, we throw an error.
    UnOp (ArithUn op) e -> do
      tell ["Evaluating unary operation: " <> Text.show op]
      let f = case op of
                Neg -> negate
                Inc -> (+ 1)
                Dec -> (\ x -> if x == 0 then 0 else x - 1)
      e' <- matchToILit =<< toNormal e
      return $ NILit (f e')

    UnOp (LogicUn op) e -> do
      tell ["Evaluating unary operation: " <> Text.show op]
      let f = case op of
                Not -> not
      e' <- matchToBLit =<< toNormal e
      return $ NBLit (f e')


    -- For a conditional, we first evaluate the condition to normal form, and then check if it's a non-zero literal. If it is, we evaluate the true branch; otherwise, we evaluate the false branch. If the condition is not a literal, we throw an error.
    Cond c t f -> do
      tell ["Evaluating conditional expression"]
      c' <- matchToBLit =<< toNormal c
      if c' then toNormal t else toNormal f

    -- For a let expression, we first evaluate the value to normal form, and then extend the environment with the new variable binding before evaluating the body. This allows us to support local variable bindings.
    Let var val body ->
      do
        val' <- toNormal val
        tell ["Binding variable: " <> var <> " = " <> Text.show val']
        local (Map.insert var val') (toNormal body)

-- | Evaluates a function application. It takes a function in normal form (which should be a closure) and a list of argument values in normal form, and applies the function to the arguments. It handles partial application by returning a new closure if not all parameters are provided, and it handles extra arguments by evaluating the function body to normal form and then applying the remaining arguments.
evalApp :: Eval m => NormalForm -> [NormalForm] -> m NormalForm
evalApp fun argValues = do
  (env, params, body) <- matchClosure fun
  let matchedParams   = List.zip params argValues
      newEnv          = List.foldl' (\acc (k, v) -> Map.insert k v acc) env matchedParams
      used            = List.length matchedParams
      remainingParams = List.drop used params
      remainingArgs   = List.drop used argValues

  case (remainingParams, remainingArgs) of
    (_:_, []) ->
      pure $ NClosure newEnv remainingParams body

    ([], []) ->
      local (const newEnv) $
        toNormal body

    ([], _ : _) ->
      local (const newEnv) $ do
        fun' <- toNormal body
        evalApp fun' remainingArgs

    (_:_, _:_ ) ->
      error "Impossible"

-- | A helper function to evaluate an expression in an empty environment and get either the result or the error.
eval :: Expr -> Either Err NormalForm
eval = fst . runEval Map.empty

-- These three functions each strip away one layer of the Eval monad stack. `runEval` runs the entire Eval monad stack, `getLogs` runs the Writer layer to get the logs, and `getErrors` runs the Except layer to get either the result or the error.

runEval :: Map Text NormalForm -> Expr -> (Either Err NormalForm, Log)
runEval env e = runReader (getLogs e) env

getLogs :: (MonadReader (Map Text NormalForm) m) => Expr -> m (Either Err NormalForm, Log)
getLogs e = runWriterT (getErrors e)

getErrors :: (MonadReader (Map Text NormalForm) m, MonadWriter Log m) =>
  Expr -> m (Either Err NormalForm)
getErrors e = runExceptT (toNormal e)

