{-# LANGUAGE QuasiQuotes #-}

module Intro.SyntaxTests where

import Intro.Quote (intro)
import Intro.Syntax (Expression (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Pretty-print"
    [ testCase "Print variable" $ do
        prettyShow [intro| a |] @?= "a",
      testCase "Print constant" $ do
        prettyShow [intro| 42 |] @?= "42",
      testCase "Print addition" $ do
        prettyShow [intro| 42 + 42 |] @?= "(42 + 42)",
      testCase "Print multiplication" $ do
        prettyShow [intro| 42 * 42 |] @?= "(42 * 42)",
      testCase "Print exponentiation" $ do
        prettyShow [intro| 2 ^ 3 |] @?= "(2 ^ 3)",
      testCase "Print subtraction with negation" $ do
        prettyShow [intro| x - - - x |] @?= "(x - (- (- x)))",
      testCase "Print compound expression" $ do
        prettyShow [intro| 2 * x + y |] @?= "((2 * x) + y)",
      testCase "Print example compound expression" $ do
        prettyShow [intro| (0 * x + 1) * 3 + 12 |] @?= "((((0 * x) + 1) * 3) + 12)",
      testCase "Print metavariable" $ do
        prettyShow [intro| $m - 1 |] @?= "(1 - 1)",
      testCase "Print precedence (right)" $ do
        prettyShow [intro| 1 + 2 * 3 |] @?= "(1 + (2 * 3))",
      testCase "Print precedence (left)" $ do
        prettyShow [intro| 1 * 2 + 3 |] @?= "((1 * 2) + 3)",
      testCase "Print addition precedence" $ do
        prettyShow [intro| x + y + z |] @?= "((x + y) + z)",
      testCase "Print subtraction precedence" $ do
        prettyShow [intro| x - y - z |] @?= "((x - y) - z)",
      testCase "Print multiplication precedence" $ do
        prettyShow [intro| x * y * z |] @?= "((x * y) * z)",
      testCase "Print exponentiation precedence" $ do
        prettyShow [intro| x ^ y ^ z |] @?= "(x ^ (y ^ z))",
      testCase "Print parentheses (left)" $ do
        prettyShow [intro| (1 + 2) * 3 |] @?= "((1 + 2) * 3)",
      testCase "Print parentheses (right)" $ do
        prettyShow [intro| 1 + (2 * 3) |] @?= "(1 + (2 * 3))"
    ]
  where
    m = Const 1
