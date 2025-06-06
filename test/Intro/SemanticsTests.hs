module Intro.SemanticsTests where

import Intro.Lexer (alexStartPos, lex)
import Intro.Parser (parseIntro)
import Intro.Semantics (simplify, simplifyWithCount)
import Intro.SemanticsTests.Data qualified as Data
import Intro.Syntax (Expression (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (lex)

parse :: String -> Expression
parse = parseIntro . lex alexStartPos

simplifyTests :: TestTree
simplifyTests =
  testGroup "Semantics.simplify" $ fmap f Data.simplify
  where
    f :: (Expression, String) -> TestTree
    f (output, input) = testCase input $ simplify (parse input) @?= output

simplifyPartialTests :: TestTree
simplifyPartialTests =
  testGroup "Semantics.simplify (partial)" $ fmap f Data.simplifyPartial
  where
    f :: (String, String) -> TestTree
    f (output, input) = testCase input $ simplify (parse input) @?= parse output

simplifyWithCountTests :: TestTree
simplifyWithCountTests =
  testGroup "Semantics.simplifyWithCount" $ fmap f Data.simplifyWithCount
  where
    f :: (Expression, Int, String) -> TestTree
    f (output, count, input) = testCase input $ simplifyWithCount (parse input) @?= (output, count)
