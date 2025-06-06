module Intro (tests) where

import Intro.QuoteTest qualified as Quote
import Intro.SemanticsTest qualified as Semantics
import Intro.SyntaxTest qualified as Syntax
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Intro"
    [ Quote.tests
    , Semantics.simplifyTests
    , Semantics.simplifyPartialTests
    , Semantics.simplifyWithCountTests
    , Syntax.tests
    ]
