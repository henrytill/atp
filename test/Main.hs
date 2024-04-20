module Main (main) where

import Intro qualified
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Tests"
    [Intro.tests]

main :: IO ()
main = defaultMain unitTests
