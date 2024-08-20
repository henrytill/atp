module PropLogic (tests) where

import PropLogic.SemanticsTests qualified as Semantics
import PropLogic.SyntaxTests qualified as Syntax
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PropLogic"
    [Syntax.tests, Semantics.tests]
