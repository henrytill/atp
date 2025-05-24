{-# LANGUAGE PatternGuards #-}

module Intro.Semantics (simplify, simplifyWithCount) where

import Control.Monad.ST (ST, runST)
import Data.Functor (($>))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Intro.Syntax (Expression (..))
import Prelude hiding (exp)

errRaiseNegative :: a
errRaiseNegative = error "cannot raise to a negative power"

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow m 1 = m
pow m n
  | n < 0 = errRaiseNegative
  | let x = pow m (n `div` 2),
    otherwise =
      x * x * if even n then 1 else m

simpl1 :: Expression -> Expression
simpl1 (Add (Const 0) x) = x
simpl1 (Add x (Const 0)) = x
simpl1 (Add (Const m) (Const n)) = Const (m + n)
simpl1 (Add (Sub e (Const m)) (Const n)) | m == n = e
simpl1 (Add (Const m) (Sub e (Const n))) | m == n = e
simpl1 (Add (Add e (Const m)) (Const n)) = Add e (Const (m + n))
simpl1 (Add (Const m) (Add e (Const n))) = Add e (Const (m + n))
simpl1 (Sub x (Const 0)) = x
simpl1 (Sub x y) | x == y = Const 0
simpl1 (Sub (Const m) (Const n)) = Const (m - n)
simpl1 (Sub (Add e (Const m)) (Const n)) | m == n = e
simpl1 (Sub (Const m) (Add e (Const n))) | m == n = Neg e
simpl1 (Sub (Sub e (Const m)) (Const n)) = Sub e (Const (m + n))
simpl1 (Mul (Const 0) _) = Const 0
simpl1 (Mul _ (Const 0)) = Const 0
simpl1 (Mul (Const 1) x) = x
simpl1 (Mul x (Const 1)) = x
simpl1 (Mul (Const m) (Const n)) = Const (m * n)
simpl1 (Exp _ (Const 0)) = Const 1
simpl1 (Exp (Const 0) _) = Const 0
simpl1 (Exp (Const 1) _) = Const 1
simpl1 (Exp x (Const 1)) = x
simpl1 (Exp _ (Neg (Const _))) = errRaiseNegative
simpl1 (Exp (Const m) (Const n)) = Const (pow m n)
simpl1 (Neg (Neg x)) = x
simpl1 (Neg (Const m)) = Const (-m)
simpl1 x = x

incr :: STRef s Int -> ST s ()
incr = flip modifySTRef succ

simplify1 :: STRef s Int -> Expression -> ST s Expression
simplify1 ref expr = incr ref $> simpl1 expr

simpl :: STRef s Int -> Expression -> ST s Expression
simpl ref expr = do
  incr ref
  case expr of
    Add (Const 0) x -> simply x
    Add x (Const 0) -> simply x
    Add x y -> add x y
    Sub x (Const 0) -> simply x
    Sub x y
      | x == y -> constant 0
      | otherwise -> sub x y
    Mul (Const 0) _ -> constant 0
    Mul _ (Const 0) -> constant 0
    Mul (Const 1) x -> simply x
    Mul x (Const 1) -> simply x
    Mul x y -> mul x y
    Exp _ (Const 0) -> constant 1
    Exp (Const 0) _ -> constant 0
    Exp (Const 1) _ -> constant 1
    Exp x (Const 1) -> simply x
    Exp _ (Neg (Const _)) -> errRaiseNegative
    Exp x y -> exp x y
    Neg (Neg x) -> simply x
    Neg x -> neg x
    Const _ -> return expr
    Var _ -> return expr
    MetaVar a -> return (MetaVar a)
  where
    binary f x y = f <$> simpl ref x <*> simpl ref y >>= simplify1 ref
    add = binary Add
    sub = binary Sub
    mul = binary Mul
    exp = binary Exp
    unary f x = simpl ref x >>= simplify1 ref . f
    neg = unary Neg
    simply = unary id
    constant = return . Const

simplifyWithCount :: Expression -> (Expression, Int)
simplifyWithCount expr = runST $ do
  ref <- newSTRef 0
  ret <- simpl ref expr
  count <- readSTRef ref
  return (ret, count)

simplify :: Expression -> Expression
simplify = fst . simplifyWithCount
