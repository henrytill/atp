{-# LANGUAGE QuasiQuotes #-}

module PropLogic.SyntaxTests where

import PropLogic.Quote (prop)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Pretty-print"
    [ testCase "Print example" $ do
        prettyShow [prop| p \/ q ==> r |] @?= "((p \\/ q) ==> r)",
      testCase "Print another example" $ do
        prettyShow [prop| p ==> q /\ ~r \/ s |] @?= "(p ==> ((q /\\ (~ r)) \\/ s))",
      testCase "Print right-associative ands" $ do
        prettyShow [prop| p /\ q /\ r |] @?= "(p /\\ (q /\\ r))",
      testCase "Print right-associative imps" $ do
        prettyShow [prop| p ==> q ==> r |] @?= "(p ==> (q ==> r))"
    ]
