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
    [ testCase "Parse variable" $ do
        [intro| a |] @?= Var "a",
      testCase "Parse constant" $ do
        [intro| 42 |] @?= Const 42,
      testCase "Parse addition" $ do
        [intro| 42 + 42 |] @?= Add (Const 42) (Const 42),
      testCase "Parse multiplication" $ do
        [intro| 42 * 42 |] @?= Mul (Const 42) (Const 42),
      testCase "Parse exponentiation" $ do
        [intro| 2 ^ 3 |] @?= Exp (Const 2) (Const 3),
      testCase "Parse subtraction with negation" $ do
        [intro| x - - - x |] @?= Sub (Var "x") (Neg (Neg (Var "x"))),
      testCase "Parse compound expression" $ do
        [intro| 2 * x + y |] @?= Add (Mul (Const 2) (Var "x")) (Var "y"),
      testCase "Parse example compound expression" $ do
        [intro| (0 * x + 1) * 3 + 12 |] @=? Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12),
      testCase "Parse metavariable" $ do
        [intro| $m - 1 |] @?= Sub (Const 1) (Const 1),
      testCase "Parse precedence (right)" $ do
        [intro| 1 + 2 * 3 |] @?= Add (Const 1) (Mul (Const 2) (Const 3)),
      testCase "Parse precedence (left)" $ do
        [intro| 1 * 2 + 3 |] @?= Add (Mul (Const 1) (Const 2)) (Const 3),
      testCase "Parse addition precedence" $ do
        [intro| x + y + z |] @?= Add (Add (Var "x") (Var "y")) (Var "z"),
      testCase "Parse subtraction precedence" $ do
        [intro| x - y - z |] @?= Sub (Sub (Var "x") (Var "y")) (Var "z"),
      testCase "Parse multiplication precedence" $ do
        [intro| x * y * z |] @?= Mul (Mul (Var "x") (Var "y")) (Var "z"),
      testCase "Parse exponentiation precedence" $ do
        [intro| x ^ y ^ z |] @?= Exp (Var "x") (Exp (Var "y") (Var "z")),
      testCase "Parse parentheses (left)" $ do
        [intro| (1 + 2) * 3 |] @?= Mul (Add (Const 1) (Const 2)) (Const 3),
      testCase "Parse parentheses (right)" $ do
        [intro| 1 + (2 * 3) |] @?= Add (Const 1) (Mul (Const 2) (Const 3))
    ]
  where
    m = Const 1
