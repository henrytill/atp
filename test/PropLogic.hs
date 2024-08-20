module PropLogic (tests) where

import PropLogic.SyntaxTests qualified as Syntax
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "PropLogic"
    [Syntax.tests]
