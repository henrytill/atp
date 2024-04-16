{-# LANGUAGE QuasiQuotes #-}

module Math.ATP.Intro.Semantics.Tests where

import Math.ATP.Intro.Quote (intro)
import Math.ATP.Intro.Semantics
import Math.ATP.Intro.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Semantics"
    [ testCase "x" $ do
        simplifyWithCount [intro| x |] @?= (Var "x", 1),
      testCase "1 + 1" $ do
        simplifyWithCount [intro| 1 + 1 |] @?= (Const 2, 4),
      testCase "(1 + 2) * (3 + 4)" $ do
        simplifyWithCount [intro| (1 + 2) * (3 + 4) |] @?= (Const 21, 10),
      testCase "0 + x" $ do
        simplifyWithCount [intro| 0 + x |] @?= (Var "x", 3),
      testCase "x + 0" $ do
        simplifyWithCount [intro| x + 0 |] @?= (Var "x", 3),
      testCase "x - 0" $ do
        simplifyWithCount [intro| x - 0 |] @?= (Var "x", 3),
      testCase "0 * x" $ do
        simplifyWithCount [intro| 0 * x |] @?= (Const 0, 1),
      testCase "x * 0" $ do
        simplifyWithCount [intro| x * 0 |] @?= (Const 0, 1),
      testCase "1 * x" $ do
        simplifyWithCount [intro| 1 * x |] @?= (Var "x", 3),
      testCase "x * 1" $ do
        simplifyWithCount [intro| x * 1 |] @?= (Var "x", 3),
      testCase "0 ^ x" $ do
        simplifyWithCount [intro| 0 ^ x |] @?= (Const 0, 1),
      testCase "x ^ 0" $ do
        simplifyWithCount [intro| x ^ 0 |] @?= (Const 1, 1),
      testCase "0 ^ 0" $ do
        simplifyWithCount [intro| 0 ^ 0 |] @?= (Const 1, 1),
      testCase "1 ^ x" $ do
        simplifyWithCount [intro| 1 ^ x |] @?= (Const 1, 1),
      testCase "x ^ 1" $ do
        simplifyWithCount [intro| x ^ 1 |] @?= (Var "x", 3),
      testCase "x - - - x" $ do
        simplifyWithCount [intro| x - - - x |] @?= (Const 0, 6),
      testCase "0 + (0 + (1 - 1))" $ do
        simplifyWithCount [intro| 0 + (0 + (1 - 1)) |] @?= (Const 0, 5),
      testCase "- - 1 - 1" $ do
        simplifyWithCount [intro| - - (1 - 1) |] @?= (Const 0, 3),
      testCase "(0 * x + 1) * 3 + 12" $ do
        simplifyWithCount [intro| (0 * x + 1) * 3 + 12 |] @?= (Const 15, 10)
    ]
