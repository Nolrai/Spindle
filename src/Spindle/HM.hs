module Spindle.HM where

import Spindle.Types
import Data.Map as Map
import Data.Set as Set
import Data.Text as Text
import Control.Monad.State.Strict
import Control.Monad.Except (MonadError (..), ExceptT, runExceptT)
import Data.List as List
import Control.Monad

infixr 0 :->

type TyVar = Integer

data HMType
  = HMInt
  | HMTyVar TyVar
  | HMType :-> HMType
  deriving (Show, Eq, Ord)

data TypeScheme = Forall (Set TyVar) HMType
  deriving (Show, Eq, Ord)

type Subst = Map TyVar HMType

type Context = Map Text TypeScheme

class Monad m => MonadFresh m where
  fresh :: m TyVar

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

-- | applies a substitution to a type, replacing type variables according to the mapping
applySubst :: Subst -> HMType -> HMType
applySubst _subst HMInt       = HMInt
applySubst subst  (HMTyVar v) = Map.findWithDefault (HMTyVar v) v subst
applySubst subst  (t1 :-> t2) = applySubst subst t1 :-> applySubst subst t2

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


algorithmW :: (MonadFresh m, MonadError [HMError] m) => Context -> Expr -> m (Subst, HMType)
-- literals have type Int
algorithmW _ (Lit _) = pure (Map.empty, HMInt)

algorithmW ctx (BiOp _ e1 e2) = do
  (s1, t1) <- algorithmW ctx e1
  (s2, t2) <- algorithmW (applySubstToContext s1 ctx) e2
  s3 <- unify (applySubst s2 t1) HMInt
  s4 <- unify (applySubst s3 t2) HMInt
  pure (s4 `o` s3 `o` s2 `o` s1, HMInt)

algorithmW ctx (UnOp _ e1) = do
  (s1, t1) <- algorithmW ctx e1
  s2 <- unify t1 HMInt
  pure (s2 `o` s1, HMInt)

algorithmW ctx (Cond e1 e2 e3) = do
  (s1, t1) <- algorithmW ctx e1
  s2 <- unify t1 HMInt
  (s3, t2) <- algorithmW (applySubstToContext s2 ctx) e2
  (s4, t3) <- algorithmW (applySubstToContext s3 (applySubstToContext s2 ctx)) e3
  s5 <- unify (applySubst s4 t2) t3
  pure (s5 `o` s4 `o` s3 `o` s2 `o` s1, applySubst s5 t2)

-- variables: look up in context and instantiate
algorithmW ctx (Var x) =
  case Map.lookup x ctx of
    Nothing -> throwError [UnboundVariable x]
    Just scheme -> do
      t <- instantiate scheme
      return (Map.empty, t)

algorithmW ctx (Lam x body) = do
  vars <- traverse (const fresh) x

  let bindings =
        Map.fromList $
          List.zip x (monoVar <$> vars)

      newCtx = ctx `Map.union` bindings

  (s1, t1) <- algorithmW newCtx body
  let lamTy = List.foldr (\v acc -> HMTyVar v :-> acc) t1 vars
  pure (s1, applySubst s1 lamTy)

algorithmW ctx (App f args) = do
  (s1, t1) <- algorithmW ctx f
  (s2, argTypes) <- foldM inferArg (s1, []) args
  resultType <- HMTyVar <$> fresh
  let funcType = List.foldr (:->) resultType argTypes
  s3 <- unify (applySubst s2 t1) funcType
  pure (s3 `o` s2 `o` s1, applySubst s3 resultType)
  where
    inferArg (s, types) arg = do
      (s', t) <- algorithmW (applySubstToContext s ctx) arg
      pure (s' `o` s, types ++ [t])

algorithmW ctx (Let x e1 e2) = do
  (s1, t1) <- algorithmW ctx e1
  let generalizedType = generalize (applySubstToContext s1 ctx) t1
      newCtx = Map.insert x generalizedType (applySubstToContext s1 ctx)
  (s2, t2) <- algorithmW newCtx e2
  pure (s2 `o` s1, t2)

runHM :: ExceptT [HMError] FreshM a -> Either [HMError] a
runHM = runFreshM . runExceptT
