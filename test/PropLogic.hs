module PropLogic (tests) where

import PropLogic.Properties qualified as Properties
import PropLogic.SemanticsTest qualified as Semantics
import PropLogic.SyntaxTest qualified as Syntax
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PropLogic"
    [Properties.tests, Semantics.tests, Syntax.tests]
