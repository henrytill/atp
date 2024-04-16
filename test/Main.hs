module Main (main) where

import Math.ATP.Intro.Tests qualified as Intro
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Tests"
    [Intro.tests]

main :: IO ()
main = defaultMain unitTests
