{-# LANGUAGE QuasiQuotes #-}

module Math.ATP.Intro.Quote.Tests where

import Math.ATP.Intro.Quote (intro)
import Math.ATP.Intro.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Quote"
    [ testCase "a" $ do
        [intro| a |] @?= Var "a",
      testCase "42" $ do
        [intro| 42 |] @?= Const 42,
      testCase "42 + 42" $ do
        [intro| 42 + 42 |] @?= Add (Const 42) (Const 42),
      testCase "42 * 42" $ do
        [intro| 42 * 42 |] @?= Mul (Const 42) (Const 42),
      testCase "2 ^ 3" $ do
        [intro| 2 ^ 3 |] @?= Exp (Const 2) (Const 3),
      testCase "x - - - x" $ do
        [intro| x - - - x |] @?= Sub (Var "x") (Neg (Neg (Var "x"))),
      testCase "2 * x + y" $ do
        [intro| 2 * x + y |] @?= Add (Mul (Const 2) (Var "x")) (Var "y"),
      testCase "(0 * x + 1) * 3 + 12" $ do
        [intro| (0 * x + 1) * 3 + 12 |] @=? Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12),
      testCase "$m - 1" $ do
        [intro| $m - 1 |] @?= Sub (Const 1) (Const 1),
      testCase "1 + 2 * 3" $ do
        [intro| 1 + 2 * 3 |] @?= Add (Const 1) (Mul (Const 2) (Const 3)),
      testCase "1 * 2 + 3" $ do
        [intro| 1 * 2 + 3 |] @?= Add (Mul (Const 1) (Const 2)) (Const 3),
      testCase "x + y + z" $ do
        [intro| x + y + z |] @?= Add (Add (Var "x") (Var "y")) (Var "z"),
      testCase "x - y - z" $ do
        [intro| x - y - z |] @?= Sub (Sub (Var "x") (Var "y")) (Var "z"),
      testCase "x * y * z" $ do
        [intro| x * y * z |] @?= Mul (Mul (Var "x") (Var "y")) (Var "z"),
      testCase "x ^ y ^ z" $ do
        [intro| x ^ y ^ z |] @?= Exp (Var "x") (Exp (Var "y") (Var "z")),
      testCase "(1 + 2) * 3" $ do
        [intro| (1 + 2) * 3 |] @?= Mul (Add (Const 1) (Const 2)) (Const 3),
      testCase "1 + (2 * 3)" $ do
        [intro| 1 + (2 * 3) |] @?= Add (Const 1) (Mul (Const 2) (Const 3))
    ]
  where
    m = Const 1
