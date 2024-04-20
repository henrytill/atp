module Intro (tests) where

import Intro.QuoteTests qualified as Quote
import Intro.SemanticsTests qualified as Semantics
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Intro"
    [Quote.tests, Semantics.tests]
