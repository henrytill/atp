{-# LANGUAGE QuasiQuotes #-}

module Intro.SyntaxTests where

import Intro.Quote (intro)
import Intro.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Pretty-print"
    [ testCase "Print variable" $ do
        pretty [intro| a |] @?= "a",
      testCase "Print constant" $ do
        pretty [intro| 42 |] @?= "42",
      testCase "Print addition" $ do
        pretty [intro| 42 + 42 |] @?= "(42 + 42)",
      testCase "Print multiplication" $ do
        pretty [intro| 42 * 42 |] @?= "(42 * 42)",
      testCase "Print exponentiation" $ do
        pretty [intro| 2 ^ 3 |] @?= "(2 ^ 3)",
      testCase "Print subtraction with negation" $ do
        pretty [intro| x - - - x |] @?= "(x - (- (- x)))",
      testCase "Print compound expression" $ do
        pretty [intro| 2 * x + y |] @?= "((2 * x) + y)",
      testCase "Print example compound expression" $ do
        pretty [intro| (0 * x + 1) * 3 + 12 |] @=? "((((0 * x) + 1) * 3) + 12)",
      testCase "Print metavariable" $ do
        pretty [intro| $m - 1 |] @?= "(1 - 1)",
      testCase "Print precedence (right)" $ do
        pretty [intro| 1 + 2 * 3 |] @?= "(1 + (2 * 3))",
      testCase "Print precedence (left)" $ do
        pretty [intro| 1 * 2 + 3 |] @?= "((1 * 2) + 3)",
      testCase "Print addition precedence" $ do
        pretty [intro| x + y + z |] @?= "((x + y) + z)",
      testCase "Print subtraction precedence" $ do
        pretty [intro| x - y - z |] @?= "((x - y) - z)",
      testCase "Print multiplication precedence" $ do
        pretty [intro| x * y * z |] @?= "((x * y) * z)",
      testCase "Print exponentiation precedence" $ do
        pretty [intro| x ^ y ^ z |] @?= "(x ^ (y ^ z))",
      testCase "Print parentheses (left)" $ do
        pretty [intro| (1 + 2) * 3 |] @?= "((1 + 2) * 3)",
      testCase "Print parentheses (right)" $ do
        pretty [intro| 1 + (2 * 3) |] @?= "(1 + (2 * 3))"
    ]
  where
    pretty = show . prettyExpression
    m = Const 1
