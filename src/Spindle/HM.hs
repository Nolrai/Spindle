module Spindle.HM where

import Spindle.Expr
import Data.Map as Map
import Data.Set as Set
import Data.Text as Text
import Control.Monad.State.Strict
import Control.Monad.Except (MonadError (..), ExceptT, runExceptT)
import Data.List as List

-- | inferHMType is The top level function for this module, which takes an expression and returns either a list of HMError if type inference fails, or the inferred HMType if it succeeds.
-- It runs the algorithmW function starting with an empty context, and extracts the resulting type from the returned tuple. The runHM function is used to handle the FreshM monad for generating fresh type variables and to manage any errors that may occur during inference.
inferHMType :: Expr -> Either [HMError] HMType
inferHMType expr = runHM $ do
  (t, _) <- algorithmW Map.empty expr
  pure t

infixr 0 :->

-- | Type variables are represented as integers, where negative integers are used for fresh type variables generated during inference.
-- This allows us to easily generate new type variables by decrementing the state, and also ensures that they do not conflict with any user-defined type variables (which are represented as non-negative integers).
type TyVar = Integer

-- | HMType represents the types in our Hindley-Milner type system.
-- It includes base types (HMInt and HMBool), type variables (HMTyVar), and function types (t1 :-> t2).
data HMType
  = HMInt
  | HMBool
  | HMTyVar TyVar
  | HMType :-> HMType
  deriving (Show, Eq, Ord)

-- | TypeScheme represents a type scheme in the Hindley-Milner type system, which consists of a set of universally quantified type variables and a base type.
-- The Forall constructor takes a set of type variables that are quantified over and an HMType that represents the body of the type scheme. This allows us to represent polymorphic types, where the quantified variables can be instantiated to different types when the scheme is used.
data TypeScheme = Forall (Set TyVar) HMType
  deriving (Show, Eq, Ord)

-- | Subst represents a substitution, which is a mapping from type variables to types. It is used during unification to keep track of the types that have been inferred for type variables, and to apply those substitutions to other types as needed.
type Subst = Map TyVar HMType

-- | Context represents the typing context, which is a mapping from variable names (Text) to their corresponding type schemes (TypeScheme). It is used during type inference to look up the types of variables and to manage the scope of type variables.
type Context = Map Text TypeScheme

-- | MonadFresh is a type class that represents monads that can generate fresh type variables. It provides a single method, fresh, which returns a new type variable each time it is called. This is essential for the type inference algorithm, as it needs to generate new type variables when inferring the types of expressions.
class Monad m => MonadFresh m where
  fresh :: m TyVar

-- | FreshM is a newtype wrapper around the State monad, which is used to implement the MonadFresh type class. The state is an integer that represents the next fresh type variable to be generated.
-- Each time fresh is called, it returns the current value of the state and then decrements it, ensuring that each generated type variable is unique and negative (to avoid conflicts with user-defined type variables).
newtype FreshM a = FreshM { runFreshM' :: State TyVar a }
  deriving (Functor, Applicative, Monad)

-- | Run a FreshM computation, starting with an initial state of -1 (fresh type variables will be generated as -1, -2, etc.)
runFreshM :: FreshM a -> a
runFreshM (FreshM m) = evalState m (-1)

-- | Generate a fresh type variable by decrementing the state and returning the old value
instance MonadFresh FreshM where
  fresh = FreshM $ state $ \s -> (s, s - 1)

instance MonadFresh m => MonadFresh (ExceptT e m) where
  fresh = lift fresh

instance MonadFresh m => MonadFresh (StateT s m) where
  fresh = lift fresh

-- | applies a substitution to a type, replacing type variables according to the mapping
applySubst :: Subst -> HMType -> HMType
applySubst subst  (HMTyVar v) = Map.findWithDefault (HMTyVar v) v subst
applySubst subst  (t1 :-> t2) = applySubst subst t1 :-> applySubst subst t2
applySubst _subst a       = a

-- | Apply substitution to a type scheme, ensuring that bound variables are not substituted
applySubstToScheme :: Subst -> TypeScheme -> TypeScheme
applySubstToScheme subst (Forall vars t) =
  let subst' = List.foldr Map.delete subst vars
  in Forall vars (applySubst subst' t)

-- | Apply substitution to a context by applying it to each type scheme in the context
applySubstToContext :: Subst -> Context -> Context
applySubstToContext subst = fmap (applySubstToScheme subst)

instantiate :: MonadFresh m => TypeScheme -> m HMType
instantiate (Forall vars t) = do
  subst <- Map.fromList <$> traverse freshBinding (Set.toList vars)
  pure (applySubst subst t)
  where
    freshBinding v = (v,) . HMTyVar <$> fresh

-- | Compute the set of free type variables in a type
freeTypeVars :: HMType -> Set TyVar
freeTypeVars HMInt = Set.empty
freeTypeVars HMBool = Set.empty
freeTypeVars (HMTyVar v) = Set.singleton v
freeTypeVars (t1 :-> t2) = freeTypeVars t1 `Set.union` freeTypeVars t2

-- | Generalize a type by quantifying over all free type variables that are not in the context
generalize :: Context -> HMType -> TypeScheme
generalize ctx t = Forall vars t
  where
    vars = freeTypeVars t `Set.difference` freeTypeVarsInContext ctx

freeTypeVarsInScheme :: TypeScheme -> Set TyVar
freeTypeVarsInScheme (Forall vars t) = freeTypeVars t `Set.difference` vars

freeTypeVarsInContext :: Context -> Set TyVar
freeTypeVarsInContext = Set.unions . fmap freeTypeVarsInScheme . Map.elems

-- | to be used as `o` operator for composing substitutions, where (s2 `o` s1) means apply s1 first, then s2
o :: Subst -> Subst -> Subst
s2 `o` s1 = Map.map (applySubst s2) s1 `Map.union` s2

data HMError
  = UnboundVariable Text
  | OccursCheckFailed TyVar HMType
  | IncompatibleTypes HMType HMType
  | WhileUnifying HMType HMType
  deriving (Show, Eq)

monoVar :: TyVar -> TypeScheme
monoVar v = Forall Set.empty (HMTyVar v)

mono :: HMType -> TypeScheme
mono = Forall Set.empty

wrapError :: MonadError [HMError] m => HMError -> m a -> m a
wrapError e action = catchError action (throwError . (e:))

unify :: MonadError [HMError] m => HMType -> HMType -> m Subst
unify HMInt HMInt = pure Map.empty
unify HMBool HMBool = pure Map.empty
unify (HMTyVar v) t = bindVar v t
unify t (HMTyVar v) = bindVar v t
unify (t1 :-> t2) (t3 :-> t4) = wrapError (WhileUnifying (t1 :-> t2) (t3 :-> t4)) $ do
  s1 <- unify t1 t3
  s2 <- unify (applySubst s1 t2) (applySubst s1 t4)
  pure (s2 `o` s1)
unify t1 t2 = throwError [IncompatibleTypes t1 t2]

bindVar :: MonadError [HMError] m => TyVar -> HMType -> m Subst
bindVar v t
  | t == HMTyVar v = pure Map.empty
  | v `Set.member` freeTypeVars t = throwError [OccursCheckFailed v t]
  | otherwise = pure (Map.singleton v t)

-- The `StateT Subst m` layer is only a local convenience for each branch of
-- `algorithmW`: it accumulates substitutions as they are produced so they can
-- be applied incrementally. Recursive calls to `algorithmW` still run in the
-- underlying monad `m`, which continues to handle fresh type variables and
-- inference errors for the whole computation. In other words, the state is not
-- threaded through recursive calls directly; it is only used to collect and
-- apply the substitutions generated within this one inference step.

inferExpr :: (MonadFresh m, MonadError [HMError] m) => Context -> Expr -> StateT Subst m HMType
inferExpr ctx e = do
  s <- get
  (t, s') <- lift $ algorithmW (applySubstToContext s ctx) e
  modify (s' `o`)
  applySubstM t

unifyM :: MonadError [HMError] m => HMType -> HMType ->  StateT Subst m HMType
unifyM t1 t2 = do
  s <- get
  s' <- lift $ unify (applySubst s t1) (applySubst s t2)
  modify (s' `o`)
  applySubstM t1

applySubstM :: MonadError [HMError] m => HMType -> StateT Subst m HMType
applySubstM t = do
  s <- get
  pure (applySubst s t)

-- | This helper function runs a StateT Subst m HMType computation starting with an empty substitution, and returns the resulting type and the final substitution.
--It is used to encapsulate the state management for each branch of algorithmW, preventing state from reaching outside of the current inference step and ensuring that substitutions are applied incrementally within each step without affecting the overall inference process.
encapsulate :: MonadError [HMError] m => StateT Subst m HMType -> m (HMType, Subst)
encapsulate action = runStateT action (Map.empty :: Subst)

algorithmW :: forall m. (MonadFresh m, MonadError [HMError] m) => Context -> Expr -> m (HMType, Subst)
-- literals have fixed type and produce no substitutions
algorithmW _ (ILit _) = pure (HMInt, Map.empty)
algorithmW _ (BLit _) = pure (HMBool, Map.empty)

-- for binary operators, we infer the types of both operands, unify them with the expected types for the operator, and return the result type
algorithmW ctx (BiOp op e1 e2) = encapsulate $ do
  t1 <- inferExpr ctx e1
  t2 <- inferExpr ctx e2
  v1 <- fresh
  let opType = case op of
        ArithOp _ -> HMInt :-> HMInt :-> HMInt
        LogicOp _ -> HMBool :-> HMBool :-> HMBool
        OrderOp _ -> HMTyVar v1 :-> HMTyVar v1 :-> HMBool

  -- we use a fresh type variable for the result type of the operator, which allows us to unify it with the expected result type (e.g. HMInt for arithmetic operators) while still allowing for polymorphism in the operand types (e.g. OrderOp can work on any type as long as both operands are the same)
  v2 <- fresh
  _ <- unifyM (t1 :-> t2 :-> HMTyVar v2) opType
  returnType <- applySubstM (HMTyVar v2)
  modify (Map.delete v2) -- remove the fresh type variable from the substitution, since it is not actually part of the inferred type
  pure returnType

-- for unary operators, we infer the type of the operand, unify it with the expected type for the operator, and return the result type
algorithmW ctx (UnOp op e1) = encapsulate $ do
  case op of
    ArithUn _ -> do
      t1 <- inferExpr ctx e1
      unifyM t1 HMInt

    LogicUn Not -> do
      t1 <- inferExpr ctx e1
      unifyM t1 HMBool

-- for conditionals, we infer the type of the guard expression and unify it with HMBool, then infer the types of the onTrue and onFalse branches and unify them with each other to ensure they have the same type, and return that type as the result
algorithmW ctx (Cond guardExpr onTrue onFalse) = encapsulate $ do
  t1 <- inferExpr ctx guardExpr
  _ <- unifyM t1 HMBool
  t2 <- inferExpr ctx onTrue
  t3 <- inferExpr ctx onFalse
  unifyM t3 t2

-- variables: look up in context and instantiate
algorithmW ctx (Var x) = encapsulate $ do
  case Map.lookup x ctx of
    Nothing -> throwError [UnboundVariable x]
    Just scheme -> instantiate scheme

algorithmW ctx (Lam x body) = encapsulate $ do
  vars <- traverse (const fresh) x

  let bindings =
        Map.fromList $
          List.zip x (monoVar <$> vars)

      newCtx = ctx `Map.union` bindings

  t1 <- inferExpr newCtx body
  let lamTy = List.foldr (\v acc -> HMTyVar v :-> acc) t1 vars
  applySubstM lamTy

algorithmW ctx (App f args) = encapsulate $ do
  t1 <- inferExpr ctx f
  argTypes <- traverse (inferExpr ctx) args
  resultType <- HMTyVar <$> fresh
  let funcType = List.foldr (:->) resultType argTypes
  _ <- unifyM t1 funcType
  applySubstM resultType

algorithmW ctx (Let x e1 e2) = encapsulate $ do
  t1 <- inferExpr ctx e1
  ctx' <- gets (`applySubstToContext` ctx)
  let generalizedType = generalize ctx' t1
      newCtx = Map.insert x generalizedType ctx'
  inferExpr newCtx e2

runHM :: ExceptT [HMError] FreshM a -> Either [HMError] a
runHM = runFreshM . runExceptT