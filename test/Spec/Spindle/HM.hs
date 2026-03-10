{-# LANGUAGE OverloadedStrings #-}

module Spec.Spindle.HM (hmTests)where

import Spindle.HM
import Data.Set as Set hiding (size)
import Hedgehog
import Hedgehog.Range as R
import Hedgehog.Gen qualified as Gen
import Control.Monad
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit (testCase, (@?=))
import Hedgehog.Gen (resize)
import Data.Either (isLeft)

toTyVar :: Size -> TyVar
toTyVar = fromIntegral

testPropertyLarge :: TestName -> Property -> TestTree
testPropertyLarge name prop = testProperty name (withTests 1000 prop)

-- | generate a random subset of 0 - size
genTyVarSet :: Gen (Set TyVar)
genTyVarSet = Gen.sized (go . toTyVar)
  where
    go :: TyVar -> Gen (Set TyVar)
    go 0 = pure Set.empty
    go size = do
      b <- Gen.bool_
      if b
        then Set.insert size <$> go (size - 1)
        else go (size - 1)

genTyVar :: Gen TyVar
genTyVar = fromIntegral <$> Gen.int (linear 0 5)

genHMType :: Gen HMType
genHMType =
  Gen.recursive Gen.choice
    [ genBase
    ]
    [ (:->) <$> genHMType <*> genHMType
    ]
  where
    genBase = do
      n <- Gen.int (linear 0 5)
      pure $
        if n == 0
        then HMInt
        else HMTyVar $ fromIntegral (n - 1)

genTypeScheme :: Gen TypeScheme
genTypeScheme = do
  body <- genHMType
  vars <- Set.fromList <$> filterM (const Gen.bool) (Set.toList $ freeTypeVars body)
  pure $ Forall vars body

genSubst :: Gen Subst
genSubst = do
  vars <- genTyVarSet
  mappings <- forM (Set.toList vars) $ \v -> do
    t <- genHMType
    pure (v, t)
  pure $ Map.fromList mappings

hmTests :: TestTree
hmTests = testGroup "hm tests"
  [ applyTests
  , compTests
  , freeTypeVarsTests
  , unifyTests
  ]

applyTests :: TestTree
applyTests = testGroup "applySubst tests"
  [ testPropertyLarge "empty subst does nothing" $
      property $ do
        hmType <- forAll (resize 1 genHMType)
        applySubst Map.empty hmType === hmType

  , testCase "simple subst" $ do
      let subst = Map.fromList [(0, HMInt)]
      applySubst subst (HMTyVar 0) @?= HMInt

  , testCase "subst with no match" $ do
      let subst = Map.fromList [(0, HMInt)]
      applySubst subst (HMTyVar 1) @?= HMTyVar 1

  , testCase "subst with function type" $ do
      let subst = Map.fromList [(0, HMInt)]
      applySubst subst (HMTyVar 0 :-> HMTyVar 1) @?= (HMInt :-> HMTyVar 1)

  , testCase "subst with nested function type" $ do
      let subst = Map.fromList [(0, HMInt), (1, HMTyVar 2)]
      applySubst subst (HMTyVar 0 :-> (HMTyVar 1 :-> HMTyVar 3))
        @?= (HMInt :-> (HMTyVar 2 :-> HMTyVar 3))

  , testPropertyLarge "subst removes variables" $
      property $ do
        subst <- forAll genSubst
        hmType <- forAll genHMType
        let applied = applySubst subst hmType
            freeVars = freeTypeVars applied
            remainingVars = freeVars `Set.difference`
              Map.foldr' (\ v acc -> Set.union acc (freeTypeVars v)) Set.empty subst
        assert (Set.null (remainingVars `Set.intersection` Map.keysSet subst))

  , testPropertyLarge "applySubstToScheme does not substitute bound variables" $
      property $ do
        subst <- forAll genSubst
        typeScheme <- forAll genTypeScheme
        let fillteredSubst = Map.filterWithKey (\k _ -> not (Set.member k (case typeScheme of Forall vars _ -> vars))) subst
        applySubstToScheme subst typeScheme === applySubstToScheme fillteredSubst typeScheme

  , testPropertyLarge "variable outside domain is unchanged" $
      property $ do
        subst <- forAll genSubst
        offset <- forAll genTyVar
        let maxKey = maybe 0 (fst . fst) (Map.maxViewWithKey subst)
            freshVar  = maxKey + offset + 1
        applySubst subst (HMTyVar freshVar) === HMTyVar freshVar
  , testPropertyLarge "applySubstToScheme preserves quantifier set" $
      property $ do
        subst <- forAll genSubst
        scheme@(Forall vars _) <- forAll genTypeScheme
        case applySubstToScheme subst scheme of
          Forall vars' _ -> vars' === vars
  ]

compTests :: TestTree
compTests = testGroup "composeSubst tests"
  [ testPropertyLarge "composing with empty subst does nothing" $
      property $ do
        (subst :: Subst) <- forAll genSubst
        footnote $ "Substitution: " ++ show subst
        let composedL = Map.empty `o` subst
        footnote $ "ComposedL: " ++ show composedL
        composedL === subst
        let composedR = subst `o` Map.empty
        footnote $ "ComposedR: " ++ show composedR
        composedR === subst

  , testPropertyLarge "composing two subst applies both" $
      property $ do
        subst1 <- forAll genSubst
        subst2 <- forAll genSubst
        let composed = subst2 `o` subst1
        hmTypes <- forAll $ Gen.list (R.linear 1 5) genHMType
        forM_ hmTypes $ \hmType ->
          applySubst composed hmType === applySubst subst2 (applySubst subst1 hmType)

  , testPropertyLarge "composition is associative extensionally" $
      property $ do
        s1 <- forAll genSubst
        s2 <- forAll genSubst
        s3 <- forAll genSubst
        t  <- forAll genHMType
        applySubst ((s3 `o` s2) `o` s1) t
          ===
          applySubst (s3 `o` (s2 `o` s1)) t
  ]

freeTypeVarsTests :: TestTree
freeTypeVarsTests = testGroup "freeTypeVars tests"
  [ freeTypeVarsInHMTypeTests
  , freeTypeVarsInSchemeTests
  , freeTypeVarsInContextTests
  ]

freeTypeVarsInHMTypeTests :: TestTree
freeTypeVarsInHMTypeTests = testGroup "freeTypeVars of HMType"
  [ testGroup "unit tests"
    [ testCase "freeTypeVars of HMInt is empty" $
        freeTypeVars HMInt @?= Set.empty
    , testCase "freeTypeVars of HMTyVar v is {v}" $
        freeTypeVars (HMTyVar 0) @?= Set.singleton 0
    , testCase "freeTypeVars of function type is union of free vars" $
        freeTypeVars (HMTyVar 0 :-> HMTyVar 1) @?= Set.fromList [0, 1]
    , testCase "freeTypeVars of nested function type is union of free vars" $
        freeTypeVars ((HMTyVar 0 :-> HMTyVar 1) :-> HMTyVar 2) @?= Set.fromList [0, 1, 2]
    ]
    , testGroup "property tests"
      [ testPropertyLarge "freeTypeVars of function type is union of free vars" $
          property $ do
            hmType1 <- forAll genHMType
            hmType2 <- forAll genHMType
            let freeVars = freeTypeVars (hmType1 :-> hmType2)
            freeVars === freeTypeVars hmType1 `Set.union` freeTypeVars hmType2
      ]
  ]

freeTypeVarsInSchemeTests :: TestTree
freeTypeVarsInSchemeTests = testGroup "freeTypeVarsInScheme tests"
  [ testGroup "unit tests"
    [ testCase "freeTypeVarsInScheme of Forall {} is free vars of body" $
        freeTypeVarsInScheme (Forall Set.empty (HMTyVar 0 :-> HMTyVar 1)) @?= Set.fromList [0, 1]
    , testCase "freeTypeVarsInScheme of Forall {0} is free vars of body except 0" $
        freeTypeVarsInScheme (Forall (Set.singleton 0) (HMTyVar 0 :-> HMTyVar 1)) @?= Set.singleton 1
    , testCase "freeTypeVarsInScheme of Forall {0,1} is free vars of body except 0 and 1" $
        freeTypeVarsInScheme (Forall (Set.fromList [0, 1]) (HMTyVar 0 :-> HMTyVar 1 :-> HMTyVar 2)) @?= Set.singleton 2
    ]
    , testGroup "property tests"
      [ testPropertyLarge "freeTypeVarsInScheme of Forall vars is free vars of body except vars" $
          property $ do
            typeScheme <- forAll genTypeScheme
            let freeVars = freeTypeVarsInScheme typeScheme
            case typeScheme of
              Forall vars t -> freeVars === freeTypeVars t `Set.difference` vars
      ]
  ]

freeTypeVarsInContextTests :: TestTree
freeTypeVarsInContextTests = testGroup "freeTypeVarsInContext tests"
  [ testGroup "unit tests"
    [ testCase "freeTypeVarsInContext of empty context is empty" $
        freeTypeVarsInContext Map.empty @?= Set.empty
    , testCase "freeTypeVarsInContext of context with one scheme is free vars of that scheme" $
        freeTypeVarsInContext (Map.fromList [("x", Forall Set.empty (HMTyVar 0 :-> HMTyVar 1))]) @?= Set.fromList [0, 1]
    , testCase "freeTypeVarsInContext of context with multiple schemes is union of free vars of those schemes" $
        freeTypeVarsInContext (Map.fromList [("x", Forall Set.empty (HMTyVar 0 :-> HMTyVar 1)), ("y", Forall Set.empty (HMTyVar 2 :-> HMTyVar 3))]) @?= Set.fromList [0, 1, 2, 3]
    ]
    , testGroup "property tests"
      [ testPropertyLarge "freeTypeVarsInContext of context is union of free vars of schemes in context" $
          property $ do
            ctx <- forAll $ Map.fromList <$> Gen.list (R.linear 1 5) ((,) <$> Gen.text (R.linear 1 5) Gen.alpha <*> genTypeScheme)
            let freeVars = freeTypeVarsInContext ctx
            freeVars === Set.unions (fmap freeTypeVarsInScheme (Map.elems ctx))
      ]
  ]

-- | Generate a random HMType that is not equal to the given HMType, used for testing that unify fails when trying to unify two different types
genHMTypeNE :: HMType -> Gen HMType
genHMTypeNE h = do
  h' <- genHMType
  if h' == h
    then genHMTypeNE h
    else pure h'

-- | Generate a random HMType that is not equal to the given HMType and also unifies with it, used for testing that unify succeeds when trying to unify two unifiable types
genHMTypeNEAndUnifies :: HMType -> Gen HMType
genHMTypeNEAndUnifies h = do
  h' <- genHMType
  if h' == h || isLeft (runHM (unify h h'))
    then genHMTypeNEAndUnifies h
    else pure h'

unifyTests :: TestTree
unifyTests = testGroup "unify tests"
  [ testGroup "unit tests"
    [ testCase "unify int with function fails" $ do
        let unifyResult = runHM (unify HMInt (HMInt :-> HMInt))
        unifyResult @?= Left [IncompatibleTypes HMInt (HMInt :-> HMInt)]

    , testCase "unify arrow types" $
        runHM (unify (HMTyVar 0 :-> HMInt) (HMInt :-> HMTyVar 1))
          @?= Right (Map.fromList [(0, HMInt), (1, HMInt)])
    ]
  , testGroup "property tests"
    [ testPropertyLarge "unify equal" $ property $ do
        a <- forAll genHMType
        runHM (unify a a) === Right mempty

    , testPropertyLarge "unify bind" $ property $ do
        a <- forAll genTyVar
        t <- forAll (genHMTypeNE (HMTyVar a))
        runHM (unify (HMTyVar a) t) === runHM (bindVar a t)

    , testPropertyLarge "unify occurs check" $ property $ do
        a <- forAll genTyVar
        t <- forAll genHMType
        let occursCheckResult = runHM (unify (HMTyVar a) (HMTyVar a :-> t))
        case occursCheckResult of
          Left (OccursCheckFailed v t' : _) -> do
            v === a
            t' === (HMTyVar a :-> t)
          _ -> failure

    , testPropertyLarge "unify is sound" $
      property $ do
        a <- forAll genHMType
        b <- forAll (genHMTypeNEAndUnifies a)
        case runHM (unify a b) of
          Right s ->
            applySubst s a === applySubst s b
          Left _ -> error "impossible case reached in unify soundness test: generated two types that do not unify"

    , testPropertyLarge "unify symmetric on success/failure" $
      property $ do
        a <- forAll genHMType
        b <- forAll genHMType
        let ab = runHM (unify a b)
            ba = runHM (unify b a)
        case (ab, ba) of
          (Right _, Right _) -> success
          (Left  _, Left  _) -> success
          _ -> failure
    ]
  ]