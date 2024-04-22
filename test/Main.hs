module Main (main) where

import Intro qualified
import Prop qualified
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Tests"
    [Intro.tests, Prop.tests]

main :: IO ()
main = defaultMain unitTests
