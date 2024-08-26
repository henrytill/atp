{-# LANGUAGE ScopedTypeVariables #-}

module PropLogic.Semantics where

import Data.Bits qualified as Bits
import Data.List qualified as List
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
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

atoms :: (Ord a) => Formula a -> [a]
atoms fm = setify $ overAtoms (:) fm []

generateTruthtable :: forall a b. (Eq a, Ord a) => ((a -> Bool) -> [a] -> b) -> [a] -> [b]
generateTruthtable subfn as =
  [subfn (valuationFor row) as | row <- [0 .. (2 ^ asLen) - 1]]
  where
    asLen :: Int
    asLen = length as

    offsetTable :: Map a Int
    offsetTable = snd $ List.foldl' (\(i, m) a -> (i - 1, Map.insert a i m)) (asLen - 1, Map.empty) as

    valuationFor :: Int -> a -> Bool
    valuationFor row a = Bits.testBit row $ offsetTable ! a

printTruthtable :: Formula Prop -> Doc
printTruthtable fm = vcat $ header : separator : body
  where
    as :: [Prop]
    as = atoms fm

    width :: Int
    width = foldr (max . length . unProp) 5 as + 1

    fixw :: String -> Doc
    fixw s = text $ s ++ replicate (width - length s) ' '

    header :: Doc
    header = foldr ((<>) . fixw . unProp) (text "| formula") as

    separator :: Doc
    separator = text $ replicate ((width * length as) + 9) '-'

    truthString :: Bool -> Doc
    truthString = fixw . show

    mkRow :: (Prop -> Bool) -> [Prop] -> Doc
    mkRow v = foldr ((<>) . truthString . v) (text "| " <> truthString (eval fm v))

    body :: [Doc]
    body = generateTruthtable mkRow as
