module Main (main) where

import Intro qualified
import PropLogic qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

unitTests :: TestTree
unitTests =
  testGroup
    "Tests"
    [Intro.tests, PropLogic.tests]

main :: IO ()
main = defaultMain unitTests
