module Math.ATP.Intro.Semantics (simplify, simplifyWithCount) where

import Control.Monad.ST
import Data.STRef
import Math.ATP.Intro.Syntax
import Prelude hiding (exp)

errRaiseNegative :: String
errRaiseNegative = "cannot raise to a negative power"

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow m 1 = m
pow m n
  | n < 0 = error errRaiseNegative
  | otherwise = x * x * if even n then 1 else m
  where
    x = pow m (n `div` 2)

simpl1 :: Expression -> Expression
simpl1 (Add (Const 0) x) = x
simpl1 (Add x (Const 0)) = x
simpl1 (Add (Const m) (Const n)) = Const (m + n)
simpl1 (Sub x (Const 0)) = x
simpl1 (Sub x y) | x == y = Const 0
simpl1 (Sub (Const m) (Const n)) = Const (m - n)
simpl1 (Mul (Const 0) _) = Const 0
simpl1 (Mul _ (Const 0)) = Const 0
simpl1 (Mul (Const 1) x) = x
simpl1 (Mul x (Const 1)) = x
simpl1 (Mul (Const m) (Const n)) = Const (m * n)
simpl1 (Exp _ (Const 0)) = Const 1
simpl1 (Exp (Const 0) _) = Const 0
simpl1 (Exp (Const 1) _) = Const 1
simpl1 (Exp x (Const 1)) = x
simpl1 (Exp _ (Neg (Const _))) = error errRaiseNegative
simpl1 (Exp (Const m) (Const n)) = Const (pow m n)
simpl1 (Neg (Neg x)) = x
simpl1 (Neg (Const m)) = Const (-m)
simpl1 x = x

incr :: STRef s Int -> ST s ()
incr = flip modifySTRef succ

simplify1 :: STRef s Int -> Expression -> ST s Expression
simplify1 ref expr = incr ref >> return (simpl1 expr)

simpl :: STRef s Int -> Expression -> ST s Expression
simpl ref expr = do
  incr ref
  case expr of
    (Add (Const 0) x) -> single x
    (Add x (Const 0)) -> single x
    (Add x y) -> add x y
    (Sub x (Const 0)) -> single x
    (Sub x y) | x == y -> zero
    (Sub x y) -> sub x y
    (Mul (Const 0) _) -> zero
    (Mul _ (Const 0)) -> zero
    (Mul (Const 1) x) -> single x
    (Mul x (Const 1)) -> single x
    (Mul x y) -> mul x y
    (Exp _ (Const 0)) -> one
    (Exp (Const 0) _) -> zero
    (Exp (Const 1) _) -> one
    (Exp x (Const 1)) -> single x
    (Exp _ (Neg (Const _))) -> error errRaiseNegative
    (Exp x y) -> exp x y
    (Neg (Neg x)) -> single x
    (Neg x) -> neg x
    (Const m) -> constant m
    (Var a) -> return (Var a)
    (MetaVar _) -> undefined
  where
    binary f x y = simplify1 ref =<< f <$> simpl ref x <*> simpl ref y
    add = binary Add
    sub = binary Sub
    mul = binary Mul
    exp = binary Exp
    unary f x = simplify1 ref . f =<< simpl ref x
    neg = unary Neg
    single = unary id
    constant = return . Const
    zero = constant 0
    one = constant 1

simplifyWithCount :: Expression -> (Expression, Int)
simplifyWithCount expr = runST $ do
  ref <- newSTRef 0
  ret <- simpl ref expr
  count <- readSTRef ref
  return (ret, count)

simplify :: Expression -> Expression
simplify = fst . simplifyWithCount
