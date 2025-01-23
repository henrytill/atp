{-# LANGUAGE ScopedTypeVariables #-}

module PropLogic.Semantics where

import Data.Bits qualified as Bits
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Monoid (All (..))
import PropLogic.Semantics.Function (Function, tryApplyWithDefault)
import PropLogic.Syntax (Atoms (..), Formula (..), Prop (..))
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

-- | Maps a function over all atoms in a formula while preserving structure
onAtoms :: (a -> Formula b) -> Formula a -> Formula b
onAtoms f = go
  where
    go FmFalse = FmFalse
    go FmTrue = FmTrue
    go (FmAtom a) = f a
    go (FmNot p) = FmNot (go p)
    go (FmAnd p q) = FmAnd (go p) (go q)
    go (FmOr p q) = FmOr (go p) (go q)
    go (FmImp p q) = FmImp (go p) (go q)
    go (FmIff p q) = FmIff (go p) (go q)
    go (FmForAll x p) = FmForAll x (go p)
    go (FmExists x p) = FmExists x (go p)
    go (FmMetaVar s) = FmMetaVar s

overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms f fm b = foldr f b (MkAtoms fm)

setify :: (Ord a) => [a] -> [a]
setify = List.sort . List.nub

atoms :: (Ord a) => Formula a -> [a]
atoms = setify . toList . MkAtoms

onAllValuations :: forall a b. (Eq a, Ord a) => ((a -> Bool) -> b) -> [a] -> [b]
onAllValuations subfn as =
  [subfn (valuationFor row) | row <- [0 .. (2 ^ asLen) - 1]]
  where
    asLen :: Int
    asLen = length as

    offsetTable :: Map a Int
    offsetTable = snd $ List.foldl' (\(i, m) a -> (pred i, Map.insert a i m)) (asLen - 1, Map.empty) as

    valuationFor :: Int -> a -> Bool
    valuationFor row a = Bits.testBit row $ offsetTable ! a

printTruthtable :: Formula Prop -> Doc
printTruthtable fm = vcat $ header : separator : body
  where
    falseLen :: Int
    falseLen = length (show False)

    as :: [Prop]
    as = atoms fm

    width :: Int
    width = succ $ foldr (max . length . unProp) falseLen as

    fixw :: String -> Doc
    fixw s = text $ s ++ replicate (width - length s) ' '

    formulaHeader :: String
    formulaHeader = "| formula"

    header :: Doc
    header = foldr ((<>) . fixw . unProp) (text formulaHeader) as

    separator :: Doc
    separator = text $ replicate ((width * length as) + length formulaHeader) '-'

    truthDoc :: Bool -> Doc
    truthDoc = fixw . show

    mkRow :: (Prop -> Bool) -> Doc
    mkRow v = foldr ((<>) . truthDoc . v) (text "| " <> truthDoc (eval fm v)) as

    body :: [Doc]
    body = onAllValuations mkRow as

tautology :: Formula Prop -> Bool
tautology fm = getAll . mconcat . onAllValuations subfn . atoms $ fm
  where
    subfn = All . eval fm

unsatisfiable :: Formula Prop -> Bool
unsatisfiable = tautology . FmNot

satisfiable :: Formula Prop -> Bool
satisfiable = not . unsatisfiable

-- | Substitutes atoms according to a partial function mapping
psubst :: (Ord a, Hashable a) => Function a (Formula a) -> Formula a -> Formula a
psubst subfn = onAtoms $ \p -> tryApplyWithDefault subfn p (FmAtom p)
