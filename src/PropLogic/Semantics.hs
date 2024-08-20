module PropLogic.Semantics where

import Data.List qualified as List
import PropLogic.Syntax

eval :: Formula Prop -> (Prop -> Bool) -> Bool
eval FmFalse _ = False
eval FmTrue _ = True
eval (FmAtom x) v = v x
eval (FmNot p) v = not (eval p v)
eval (FmAnd p q) v = eval p v && eval q v
eval (FmOr p q) v = eval p v || eval q v
eval (FmImp p q) v = not (eval p v) || eval q v
eval (FmIff p q) v = eval p v == eval q v
eval (FmForAll {}) _ = undefined
eval (FmExists {}) _ = undefined
eval (FmMetaVar {}) _ = undefined

overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms f fm b = case fm of
  FmAtom a -> f a b
  FmFalse -> b
  FmTrue -> b
  FmNot p -> overAtoms f p b
  FmAnd p q -> overAtoms f p (overAtoms f q b)
  FmOr p q -> overAtoms f p (overAtoms f q b)
  FmImp p q -> overAtoms f p (overAtoms f q b)
  FmIff p q -> overAtoms f p (overAtoms f q b)
  FmForAll _ p -> overAtoms f p b
  FmExists _ p -> overAtoms f p b
  FmMetaVar _ -> undefined

setify :: (Ord a) => [a] -> [a]
setify = List.sort . List.nub

atomUnion :: (Ord b) => (a -> [b]) -> Formula a -> [b]
atomUnion f fm = setify $ overAtoms ((++) . f) fm []

atoms :: (Ord a) => Formula a -> [a]
atoms = atomUnion (: [])
