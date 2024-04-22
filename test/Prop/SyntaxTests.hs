{-# LANGUAGE QuasiQuotes #-}

module Prop.SyntaxTests where

import Prop.Quote (prop)
import Prop.Syntax
import Test.Tasty
import Test.Tasty.HUnit

pretty :: Formula Prop -> String
pretty = show . prettyFormula

tests :: TestTree
tests =
  testGroup
    "Pretty-print"
    [ testCase "Print example" $ do
        pretty [prop| p \/ q ==> r |] @?= "((p \\/ q) ==> r)",
      testCase "Print another example" $ do
        pretty [prop| p ==> q /\ ~r \/ s |] @?= "(p ==> ((q /\\ (~ r)) \\/ s))",
      testCase "Print right-associative ands" $ do
        pretty [prop| p /\ q /\ r |] @?= "(p /\\ (q /\\ r))",
      testCase "Print right-associative imps" $ do
        pretty [prop| p ==> q ==> r |] @?= "(p ==> (q ==> r))"
    ]
