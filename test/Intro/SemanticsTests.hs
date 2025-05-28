module Intro.SemanticsTests where

import Intro.Lexer (alexStartPos, lex)
import Intro.Parser (parseIntro)
import Intro.Semantics (simplify, simplifyWithCount)
import Intro.Syntax (Expression (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (lex)

parse :: String -> Expression
parse = parseIntro . lex alexStartPos

parseSimplify :: String -> Expression
parseSimplify = simplify . parse

parseSimplifyWithCount :: String -> (Expression, Int)
parseSimplifyWithCount = simplifyWithCount . parse

simplifyTests :: TestTree
simplifyTests = testGroup "Semantics.simplify" $ fmap f inputs
  where
    inputs =
      [ (Const 7, "1 + 2 * 3")
      , (Const 21, "(1 + 2) * (3 + 4)")
      , (Const 15, "(0 * x + 1) * 3 + 12")
      , (Const 0, "0 + (0 + (1 - 1))")
      , (Const 0, "z * (0 * (x * y))")
      , (Const 8, "2 ^ (1 + 2)")
      , (Var "x", "5 + (x - 5)")
      , (Var "x", "3 + ((x - 1) - 2)")
      , (Var "x", "((x * 1) + 0) - ((y - y) * z)")
      , (Const 1, "1 + ((x - x) * (y + z))")
      ]
    f :: (Expression, String) -> TestTree
    f (output, input) = testCase input $ parseSimplify input @?= output

simplifyPartialTests :: TestTree
simplifyPartialTests = testGroup "Semantics.simplify (partial)" $ fmap f inputs
  where
    inputs =
      [ ("x + 15", "x + 15 - 12 * 0")
      , ("-x", "-(-(-(x)))")
      , ("x + y", "0 + (x + (0 + y))")
      , ("x * y", "1 * (x * (1 * y))")
      , ("x - (y - (y - x))", "x - (y - (y - x))")
      , ("x + 1", "(x + 0) * (1 + (y - y)) + (z ^ 0)")
      , ("x + z", "(x + 0) * (1 + (y - y)) + (z ^ 1)")
      , ("x + 3", "((((x + 1) - 1) + 2) - 2) + 3")
      , -- Tests for c1 + (x - c2) -> x when c1 == c2
        ("y + 3", "7 + ((y + 3) - 7)")
      , -- Tests for c1 - (x + c2) -> -x when c1 == c2
        ("-z", "4 - (z + 4)")
      , ("-(a * b)", "10 - ((a * b) + 10)")
      , -- More complex nested cases
        ("-y", "5 - ((3 + (y + 2)))")
      , ("x * (y + z)", "x * (y + (z * (2 - 1))) + (0 * w)")
      , ("x * y", "(x * (y + 0)) + (0 * z)")
      , ("x * y", "x * (y ^ ((0 + 2) - 1))")
      ]
    f :: (String, String) -> TestTree
    f (output, input) = testCase input $ parseSimplify input @?= parse output

simplifyWithCountTests :: TestTree
simplifyWithCountTests = testGroup "Semantics.simplifyWithCount" $ fmap f inputs
  where
    inputs =
      [ (Var "x", 1, "x")
      , (Const 2, 4, "1 + 1")
      , (Const 21, 10, "(1 + 2) * (3 + 4)")
      , (Var "x", 3, "0 + x")
      , (Var "x", 3, "x + 0")
      , (Var "x", 3, "x - 0")
      , (Const 0, 1, "0 * x")
      , (Const 0, 1, "x * 0")
      , (Var "x", 3, "1 * x")
      , (Var "x", 3, "x * 1")
      , (Const 0, 1, "0 ^ x")
      , (Const 1, 1, "x ^ 0")
      , (Const 1, 1, "0 ^ 0")
      , (Const 1, 1, "1 ^ x")
      , (Var "x", 3, "x ^ 1")
      , (Const 0, 6, "x - - - x")
      , (Const 0, 5, "0 + (0 + (1 - 1))")
      , (Const 0, 3, "- - (1 - 1)")
      , (Const 15, 10, "(0 * x + 1) * 3 + 12")
      ]
    f :: (Expression, Int, String) -> TestTree
    f (output, count, input) = testCase input $ parseSimplifyWithCount input @?= (output, count)
