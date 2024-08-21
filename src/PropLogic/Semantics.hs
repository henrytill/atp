module PropLogic.Semantics where

import Data.List qualified as List
import Data.Monoid (All (..))
import PropLogic.Syntax (Formula (..), Prop (..))
import Text.PrettyPrint (Doc, text, vcat, (<>))
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

accumAllValuations :: (Eq a) => ((a -> Bool) -> b) -> (a -> Bool) -> [a] -> [b]
accumAllValuations subfn v ats = case ats of
  [] -> [subfn v]
  p : ps ->
    accumAllValuations subfn (v' False) ps ++ accumAllValuations subfn (v' True) ps
    where
      v' t q
        | q == p = t
        | otherwise = v q

onAllValuations :: (Eq a) => ((a -> Bool) -> Bool) -> (a -> Bool) -> [a] -> Bool
onAllValuations subfn v = getAll . mconcat . accumAllValuations subfn' v
  where
    subfn' = All . subfn

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
    body = accumAllValuations mkRow (const False) ats
