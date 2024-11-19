{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PropLogic.Properties where

import Data.Foldable qualified as Foldable
import Data.Monoid
import PropLogic.Syntax (Atoms (..), Formula (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QuickCheck

instance (Arbitrary a) => Arbitrary (Formula a) where
  arbitrary = sized genFormula
    where
      genFormula 0 = oneof [pure FmTrue, pure FmFalse, FmAtom <$> arbitrary]
      genFormula n =
        oneof
          [ pure FmTrue,
            pure FmFalse,
            FmAtom <$> arbitrary,
            FmNot <$> genFormula',
            FmAnd <$> genFormula' <*> genFormula',
            FmOr <$> genFormula' <*> genFormula',
            FmImp <$> genFormula' <*> genFormula',
            FmIff <$> genFormula' <*> genFormula'
          ]
        where
          genFormula' = genFormula (n `div` 2)

instance (Arbitrary a) => Arbitrary (Atoms a) where
  arbitrary = MkAtoms <$> arbitrary

type TestFormula = Formula Int

type TestAcc = [Int]

type TestFun = Fun Int TestAcc

prop_foldr_foldMap :: TestFormula -> TestFun -> TestAcc -> Property
prop_foldr_foldMap fm (Fun _ f) z =
  foldr g z (MkAtoms fm) === appEndo (foldMap (Endo . g) (MkAtoms fm)) z
  where
    g x acc = f x <> acc

prop_foldMap_fold_fmap :: TestFormula -> TestFun -> Property
prop_foldMap_fold_fmap fm (Fun _ f) =
  foldMap f (MkAtoms fm) === Foldable.fold (fmap f (MkAtoms fm))

prop_foldl_foldMap :: TestFormula -> TestFun -> TestAcc -> Property
prop_foldl_foldMap fm (Fun _ f) z =
  foldl (\acc x -> acc <> f x) z (MkAtoms fm) === appEndo (getDual (foldMap (Dual . Endo . (\x acc -> acc <> f x)) (MkAtoms fm))) z

tests :: TestTree
tests =
  testGroup
    "Foldable Laws for Atoms"
    [ QuickCheck.testProperty "foldr-foldMap" prop_foldr_foldMap,
      QuickCheck.testProperty "foldMap-fold-fmap" prop_foldMap_fold_fmap,
      QuickCheck.testProperty "foldl-foldMap" prop_foldl_foldMap
    ]
