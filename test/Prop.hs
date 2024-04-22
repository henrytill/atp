module Prop (tests) where

import Prop.SyntaxTests qualified as Syntax
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Prop"
    [Syntax.tests]
