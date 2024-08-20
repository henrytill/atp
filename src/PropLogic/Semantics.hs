module PropLogic.Semantics where

import Data.List qualified as List
import PropLogic.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

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

onAllValuations :: (Eq a) => ((a -> Bool) -> b) -> (a -> Bool) -> [a] -> [b]
onAllValuations subfn v ats = case ats of
  [] -> [subfn v]
  p : ps ->
    let v' t q = if q == p then t else v q
     in onAllValuations subfn (v' False) ps ++ onAllValuations subfn (v' True) ps

printTruthtable :: Formula Prop -> Doc
printTruthtable fm = vcat $ header : separator : body
  where
    ats :: [Prop]
    ats = atoms fm

    width :: Int
    width = foldr (max . length . unProp) 5 ats + 1

    fixw :: String -> Doc
    fixw s = text $ s ++ replicate (width - length s) ' '

    header :: Doc
    header = foldr ((<>) . fixw . unProp) (text "| formula") ats

    separator :: Doc
    separator = text $ replicate ((width * length ats) + 9) '-'

    truthString :: Bool -> Doc
    truthString = fixw . show

    mkRow :: (Prop -> Bool) -> Doc
    mkRow v = foldr ((<>) . truthString . v) (text "| " <> truthString (eval fm v)) ats

    body :: [Doc]
    body = onAllValuations mkRow (const False) ats
