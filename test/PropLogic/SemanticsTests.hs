{-# LANGUAGE QuasiQuotes #-}

module PropLogic.SemanticsTests where

import PropLogic.Quote (prop)
import PropLogic.Semantics
import PropLogic.Syntax
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Semantics"
    [ testCase "Parse and eval example" $ do
        let v (MkProp "p") = True
            v (MkProp "q") = False
            v (MkProp "r") = True
            v _ = undefined
        eval [prop| p /\ q ==> q /\ r |] v @?= True,
      testCase "Parse and eval example" $ do
        let v (MkProp "p") = True
            v (MkProp "q") = True
            v (MkProp "r") = False
            v _ = undefined
        eval [prop| p /\ q ==> q /\ r |] v @?= False,
      testCase "Check that setify removes duplicates and sorts" $ do
        setify ([1, 2, 3, 1, 4, 3] :: [Int]) @?= [1, 2, 3, 4],
      testCase "Check that setify reverses" $ do
        setify ([4, 3, 2, 1] :: [Int]) @?= [1, 2, 3, 4],
      testCase "Check atoms against example" $ do
        atoms [prop| p /\ q \/ s ==> ~p \/ (r <=> s) |] @?= MkProp <$> ["p", "q", "r", "s"]
    ]
