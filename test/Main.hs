module Main (main) where

import Math.ATP.Intro.Quote.Tests qualified as Quote
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [Quote.tests]

main :: IO ()
main = defaultMain unitTests
