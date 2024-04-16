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

simplify1 :: Expression -> Expression
simplify1 (Add (Const 0) e) = e
simplify1 (Add e (Const 0)) = e
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Sub e (Const 0)) = e
simplify1 (Sub e1 e2) | e1 == e2 = Const 0
simplify1 (Sub (Const m) (Const n)) = Const (m - n)
simplify1 (Mul (Const 0) _) = Const 0
simplify1 (Mul _ (Const 0)) = Const 0
simplify1 (Mul (Const 1) e) = e
simplify1 (Mul e (Const 1)) = e
simplify1 (Mul (Const m) (Const n)) = Const (m * n)
simplify1 (Exp _ (Const 0)) = Const 1
simplify1 (Exp (Const 0) _) = Const 0
simplify1 (Exp (Const 1) _) = Const 1
simplify1 (Exp e (Const 1)) = e
simplify1 (Exp (Const m) (Const n)) = Const (pow m n)
simplify1 (Exp (Const _) (Neg (Const _))) = error errRaiseNegative
simplify1 (Neg (Neg e)) = e
simplify1 (Neg (Const m)) = Const (-m)
simplify1 e = e

incr :: STRef s Int -> ST s ()
incr = flip modifySTRef succ

simplify1WithCount :: STRef s Int -> Expression -> ST s Expression
simplify1WithCount ref expr = incr ref >> return (simplify1 expr)

simpl :: STRef s Int -> Expression -> ST s Expression
simpl ref expr = do
  incr ref
  case expr of
    (Add (Const 0) e) -> single e
    (Add e (Const 0)) -> single e
    (Add e1 e2) -> add e1 e2
    (Sub e (Const 0)) -> single e
    (Sub e1 e2) | e1 == e2 -> zero
    (Sub e1 e2) -> sub e1 e2
    (Mul (Const 0) _) -> zero
    (Mul _ (Const 0)) -> zero
    (Mul (Const 1) e) -> single e
    (Mul e (Const 1)) -> single e
    (Mul e1 e2) -> mul e1 e2
    (Exp (Const 0) _) -> zero
    (Exp _ (Const 0)) -> one
    (Exp (Const 1) _) -> one
    (Exp e (Const 1)) -> single e
    (Exp e1 e2) -> exp e1 e2
    (Neg (Neg e)) -> single e
    (Neg e) -> neg e
    (Const m) -> constant m
    (Var a) -> return (Var a)
    (MetaVar _) -> undefined
  where
    binary f x y = simplify1WithCount ref =<< f <$> simpl ref x <*> simpl ref y
    add = binary Add
    sub = binary Sub
    mul = binary Mul
    exp = binary Exp
    unary f x = simplify1WithCount ref =<< f <$> simpl ref x
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
