module Math.ATP.Intro.Semantics (simplify, simplifyWithCount) where

import Control.Monad.ST
import Data.STRef
import Math.ATP.Intro.Syntax

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
incr ref = modifySTRef ref succ

simplify1WithCount :: STRef s Int -> Expression -> ST s Expression
simplify1WithCount ref expr = incr ref >> return (simplify1 expr)

simplifyWithCount :: Expression -> (Expression, Int)
simplifyWithCount expr = runST $ do
  ref <- newSTRef 0
  ret <- go ref expr
  count <- readSTRef ref
  return (ret, count)
  where
    go :: STRef s Int -> Expression -> ST s Expression
    go ref expr' = do
      incr ref
      case expr' of
        (Add e1 e2) -> Add <$> go ref e1 <*> go ref e2 >>= simplify1WithCount ref
        (Sub e1 e2) -> Sub <$> go ref e1 <*> go ref e2 >>= simplify1WithCount ref
        (Mul e1 e2) -> Mul <$> go ref e1 <*> go ref e2 >>= simplify1WithCount ref
        (Exp e1 e2) -> Exp <$> go ref e1 <*> go ref e2 >>= simplify1WithCount ref
        (Neg e) -> Neg <$> go ref e >>= simplify1WithCount ref
        (Const m) -> return $ Const m
        (Var a) -> return $ Var a
        (MetaVar _) -> undefined

simplify :: Expression -> Expression
simplify = fst . simplifyWithCount
