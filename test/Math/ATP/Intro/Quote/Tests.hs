{-# LANGUAGE QuasiQuotes #-}

module Math.ATP.Intro.Quote.Tests where

import Math.ATP.Intro.Quote (intro)
import Math.ATP.Intro.Syntax
import Test.Tasty
import Test.Tasty.HUnit

parseVar :: TestTree
parseVar = testCase "a" $ do
  [intro| a |] @?= Var "a"

parseConst :: TestTree
parseConst = testCase "42" $ do
  [intro| 42 |] @?= Const 42

parseAdd :: TestTree
parseAdd = testCase "42 + 42" $ do
  [intro| 42 + 42 |] @?= Add (Const 42) (Const 42)

parseMul :: TestTree
parseMul = testCase "42 * 42" $ do
  [intro| 42 * 42 |] @?= Mul (Const 42) (Const 42)

parseExp :: TestTree
parseExp = testCase "2 ^ 3" $ do
  [intro| 2 ^ 3 |] @?= Exp (Const 2) (Const 3)

parseSubNeg :: TestTree
parseSubNeg = testCase "x - - - x" $ do
  [intro| x - - - x |] @?= Sub (Var "x") (Neg (Neg (Var "x")))

parseMulAdd :: TestTree
parseMulAdd = testCase "2 * x + y" $ do
  [intro| 2 * x + y |] @?= Add (Mul (Const 2) (Var "x")) (Var "y")

parseExample :: TestTree
parseExample = testCase "(0 * x + 1) * 3 + 12" $ do
  [intro| (0 * x + 1) * 3 + 12 |] @=? Add (Mul (Add (Mul (Const 0) (Var "x")) (Const 1)) (Const 3)) (Const 12)

parseMeta :: TestTree
parseMeta = testCase "$m - 1" $ do
  [intro| $m - 1 |] @?= Sub (Const 1) (Const 1)
  where
    m = Const 1

parsePrecRight :: TestTree
parsePrecRight = testCase "1 + 2 * 3" $ do
  [intro| 1 + 2 * 3 |] @?= Add (Const 1) (Mul (Const 2) (Const 3))

parsePrecLeft :: TestTree
parsePrecLeft = testCase "1 * 2 + 3" $ do
  [intro| 1 * 2 + 3 |] @?= Add (Mul (Const 1) (Const 2)) (Const 3)

parsePrecAdd :: TestTree
parsePrecAdd = testCase "x + y + z" $ do
  [intro| x + y + z |] @?= Add (Add (Var "x") (Var "y")) (Var "z")

parsePrecSub :: TestTree
parsePrecSub = testCase "x - y - z" $ do
  [intro| x - y - z |] @?= Sub (Sub (Var "x") (Var "y")) (Var "z")

parsePrecMul :: TestTree
parsePrecMul = testCase "x * y * z" $ do
  [intro| x * y * z |] @?= Mul (Mul (Var "x") (Var "y")) (Var "z")

parsePrecExp :: TestTree
parsePrecExp = testCase "x ^ y ^ z" $ do
  [intro| x ^ y ^ z |] @?= Exp (Var "x") (Exp (Var "y") (Var "z"))

parsePrecParensLeft :: TestTree
parsePrecParensLeft = testCase "(1 + 2) * 3" $ do
  [intro| (1 + 2) * 3 |] @?= Mul (Add (Const 1) (Const 2)) (Const 3)

parsePrecParensRight :: TestTree
parsePrecParensRight = testCase "1 + (2 * 3)" $ do
  [intro| 1 + (2 * 3) |] @?= Add (Const 1) (Mul (Const 2) (Const 3))

tests :: TestTree
tests =
  testGroup
    "Quote"
    [ parseVar,
      parseConst,
      parseAdd,
      parseMul,
      parseExp,
      parseSubNeg,
      parseMulAdd,
      parseExample,
      parseMeta,
      parsePrecRight,
      parsePrecLeft,
      parsePrecAdd,
      parsePrecSub,
      parsePrecMul,
      parsePrecExp,
      parsePrecParensLeft,
      parsePrecParensRight
    ]
