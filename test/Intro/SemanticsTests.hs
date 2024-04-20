{-# LANGUAGE QuasiQuotes #-}

module Intro.SemanticsTests where

import Intro.Quote (intro)
import Intro.Semantics
import Intro.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Semantics"
    [ testCase "Simplify variable" $ do
        simplifyWithCount [intro| x |] @?= (Var "x", 1),
      testCase "Simplify addition" $ do
        simplifyWithCount [intro| 1 + 1 |] @?= (Const 2, 4),
      testCase "Simplify compound expression" $ do
        simplifyWithCount [intro| (1 + 2) * (3 + 4) |] @?= (Const 21, 10),
      testCase "Simplify add to 0 (left)" $ do
        simplifyWithCount [intro| 0 + x |] @?= (Var "x", 3),
      testCase "Simplify add to 0 (right)" $ do
        simplifyWithCount [intro| x + 0 |] @?= (Var "x", 3),
      testCase "Simplify subtract 0" $ do
        simplifyWithCount [intro| x - 0 |] @?= (Var "x", 3),
      testCase "Simplify multiply by 0 (left)" $ do
        simplifyWithCount [intro| 0 * x |] @?= (Const 0, 1),
      testCase "Simplify multiply by 0 (right)" $ do
        simplifyWithCount [intro| x * 0 |] @?= (Const 0, 1),
      testCase "Simplify multiply by 1 (left)" $ do
        simplifyWithCount [intro| 1 * x |] @?= (Var "x", 3),
      testCase "Simplify multiply by 1 (right)" $ do
        simplifyWithCount [intro| x * 1 |] @?= (Var "x", 3),
      testCase "Simplify 0 raised to x" $ do
        simplifyWithCount [intro| 0 ^ x |] @?= (Const 0, 1),
      testCase "Simplify x raised to 0" $ do
        simplifyWithCount [intro| x ^ 0 |] @?= (Const 1, 1),
      testCase "Simplify 0 raised to 0" $ do
        simplifyWithCount [intro| 0 ^ 0 |] @?= (Const 1, 1),
      testCase "Simplify 1 raised to x" $ do
        simplifyWithCount [intro| 1 ^ x |] @?= (Const 1, 1),
      testCase "Simplify x raised to 1" $ do
        simplifyWithCount [intro| x ^ 1 |] @?= (Var "x", 3),
      testCase "Simplify subtraction with negation" $ do
        simplifyWithCount [intro| x - - - x |] @?= (Const 0, 6),
      testCase "Simplify some short circuits (a)" $ do
        simplifyWithCount [intro| 0 + (0 + (1 - 1)) |] @?= (Const 0, 5),
      testCase "Simplify some short circuits (b)" $ do
        simplifyWithCount [intro| - - (1 - 1) |] @?= (Const 0, 3),
      testCase "Simplify example compound expression" $ do
        simplifyWithCount [intro| (0 * x + 1) * 3 + 12 |] @?= (Const 15, 10)
    ]
