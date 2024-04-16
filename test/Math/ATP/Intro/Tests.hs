module Math.ATP.Intro.Tests where

import Math.ATP.Intro.Quote.Tests qualified as Quote
import Math.ATP.Intro.Semantics.Tests qualified as Semantics
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Intro"
    [Quote.tests, Semantics.tests]
