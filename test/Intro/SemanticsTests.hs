{-# LANGUAGE QuasiQuotes #-}

module Intro.SemanticsTests where

import Intro.Lexer (AlexPosn (..), lex)
import Intro.Parser (parseIntro)
import Intro.Quote (intro)
import Intro.Semantics (simplify, simplifyWithCount)
import Intro.Syntax (Expression (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (lex)

tests :: TestTree
tests =
  testGroup
    "Semantics"
    [ testCase "Simplify variable" $ do
        simplifyWithCount [intro| x |] @?= (Var "x", 1)
    , testCase "Simplify addition" $ do
        simplifyWithCount [intro| 1 + 1 |] @?= (Const 2, 4)
    , testCase "Simplify compound expression" $ do
        simplifyWithCount [intro| (1 + 2) * (3 + 4) |] @?= (Const 21, 10)
    , testCase "Simplify add to 0 (left)" $ do
        simplifyWithCount [intro| 0 + x |] @?= (Var "x", 3)
    , testCase "Simplify add to 0 (right)" $ do
        simplifyWithCount [intro| x + 0 |] @?= (Var "x", 3)
    , testCase "Simplify subtract 0" $ do
        simplifyWithCount [intro| x - 0 |] @?= (Var "x", 3)
    , testCase "Simplify multiply by 0 (left)" $ do
        simplifyWithCount [intro| 0 * x |] @?= (Const 0, 1)
    , testCase "Simplify multiply by 0 (right)" $ do
        simplifyWithCount [intro| x * 0 |] @?= (Const 0, 1)
    , testCase "Simplify multiply by 1 (left)" $ do
        simplifyWithCount [intro| 1 * x |] @?= (Var "x", 3)
    , testCase "Simplify multiply by 1 (right)" $ do
        simplifyWithCount [intro| x * 1 |] @?= (Var "x", 3)
    , testCase "Simplify 0 raised to x" $ do
        simplifyWithCount [intro| 0 ^ x |] @?= (Const 0, 1)
    , testCase "Simplify x raised to 0" $ do
        simplifyWithCount [intro| x ^ 0 |] @?= (Const 1, 1)
    , testCase "Simplify 0 raised to 0" $ do
        simplifyWithCount [intro| 0 ^ 0 |] @?= (Const 1, 1)
    , testCase "Simplify 1 raised to x" $ do
        simplifyWithCount [intro| 1 ^ x |] @?= (Const 1, 1)
    , testCase "Simplify x raised to 1" $ do
        simplifyWithCount [intro| x ^ 1 |] @?= (Var "x", 3)
    , testCase "Simplify subtraction with negation" $ do
        simplifyWithCount [intro| x - - - x |] @?= (Const 0, 6)
    , testCase "Simplify some short circuits (a)" $ do
        simplifyWithCount [intro| 0 + (0 + (1 - 1)) |] @?= (Const 0, 5)
    , testCase "Simplify some short circuits (b)" $ do
        simplifyWithCount [intro| - - (1 - 1) |] @?= (Const 0, 3)
    , testCase "Simplify example compound expression" $ do
        simplifyWithCount [intro| (0 * x + 1) * 3 + 12 |] @?= (Const 15, 10)
    ]

moreTests :: TestTree
moreTests =
  let inputs =
        [ ("7", "1 + 2 * 3")
        , ("21", "(1 + 2) * (3 + 4)")
        , ("15", "(0 * x + 1) * 3 + 12")
        , ("0", "0 + (0 + (1 - 1))")
        , ("x + 15", "x + 15 - 12 * 0")
        , ("-x", "-(-(-(x)))")
        , ("x  + y", "0 + (x + (0 + y))")
        , ("x * y", "1 * (x * (1 * y))")
        , ("0", "z * (0 * (x * y))")
        , ("x - (y - (y - x))", "x - (y - (y - x))")
        , ("8", "2 ^ (1 + 2)")
        , ("x + 1", "(x + 0) * (1 + (y - y)) + (z ^ 0)")
        , ("x + z", "(x + 0) * (1 + (y - y)) + (z ^ 1)")
        , ("x + 3", "((((x + 1) - 1) + 2) - 2) + 3")
        , -- Tests for c1 + (x - c2) -> x when c1 == c2
          ("x", "5 + (x - 5)")
        , ("y + 3", "7 + ((y + 3) - 7)")
        , -- Tests for c1 - (x + c2) -> -x when c1 == c2
          ("-z", "4 - (z + 4)")
        , ("-(a * b)", "10 - ((a * b) + 10)")
        , -- More complex nested cases
          ("x", "3 + ((x - 1) - 2)")
        , ("-y", "5 - ((3 + (y + 2)))")
        , ("x * (y + z)", "x * (y + (z * (2 - 1))) + (0 * w)")
        , ("x * y", "(x * (y + 0)) + (0 * z)")
        , ("x * y", "x * (y ^ ((0 + 2) - 1))")
        , ("x", "((x * 1) + 0) - ((y - y) * z)")
        , ("1", "1 + ((x - x) * (y + z))")
        ]
      f :: (String, String) -> TestTree
      f (expectedStr, actualStr) = testCase actualStr $ actual @?= expected
        where
          p :: AlexPosn
          p = AlexPn 0 0 0
          expected, actual :: Expression
          expected = parseIntro . lex p $ expectedStr
          actual = simplify . parseIntro . lex p $ actualStr
   in testGroup "Semantics (more)" $ fmap f inputs
