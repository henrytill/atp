module Intro (tests) where

import Intro.QuoteTests qualified as Quote
import Intro.SemanticsTests qualified as Semantics
import Intro.SyntaxTests qualified as Syntax
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Intro"
    [Quote.tests, Semantics.tests, Syntax.tests]
