{-# LANGUAGE ScopedTypeVariables #-}

module PropLogic.Semantics where

import Control.Monad
import Data.Bits qualified as Bits
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Monoid (All (..))
import PropLogic.Semantics.Function (Function, tryApplyWithDefault)
import PropLogic.Syntax (Atoms (..), Formula (..), Prop (..), onAtoms)
import Text.PrettyPrint (Doc, text, vcat, (<>))
import Prelude hiding ((<>))

eval :: Formula a -> (a -> Bool) -> Bool
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

simplify1 :: Formula a -> Formula a
simplify1 (FmNot FmFalse) = FmTrue
simplify1 (FmNot FmTrue) = FmFalse
simplify1 (FmNot (FmNot p)) = p
simplify1 (FmAnd _ FmFalse) = FmFalse
simplify1 (FmAnd FmFalse _) = FmFalse
simplify1 (FmAnd p FmTrue) = p
simplify1 (FmAnd FmTrue p) = p
simplify1 (FmOr p FmFalse) = p
simplify1 (FmOr FmFalse p) = p
simplify1 (FmOr _ FmTrue) = FmTrue
simplify1 (FmOr FmTrue _) = FmTrue
simplify1 (FmImp FmFalse _) = FmTrue
simplify1 (FmImp _ FmTrue) = FmTrue
simplify1 (FmImp FmTrue p) = p
simplify1 (FmImp p FmFalse) = FmNot p
simplify1 (FmIff p FmTrue) = p
simplify1 (FmIff FmTrue p) = p
simplify1 (FmIff p FmFalse) = FmNot p
simplify1 (FmIff FmFalse p) = FmNot p
simplify1 fm = fm

simplify :: Formula a -> Formula a
simplify fm =
  case fm of
    FmNot p -> unary FmNot p
    FmAnd p q -> binary FmAnd p q
    FmOr p q -> binary FmOr p q
    FmImp p q -> binary FmImp p q
    FmIff p q -> binary FmIff p q
    _ -> fm
  where
    unary f p = simplify1 $ f (simplify p)
    binary f p q = simplify1 $ f (simplify p) (simplify q)

setify :: (Ord a) => [a] -> [a]
setify = map NonEmpty.head . NonEmpty.group . List.sort

atoms :: (Ord a) => Formula a -> [a]
atoms = setify . toList . MkAtoms

onAllValuations :: forall a b. (Eq a, Ord a) => ((a -> Bool) -> b) -> [a] -> [b]
onAllValuations subfn as =
  [subfn (valuationFor row) | row <- [0 .. pred numValuations]]
  where
    asLen :: Int
    asLen = length as

    offsetTable :: Map a Int
    offsetTable =
      as
        & List.foldl'
          (\(i, m) a -> (pred i, Map.insert a i m))
          (pred asLen, Map.empty)
        & snd

    valuationFor :: Int -> a -> Bool
    valuationFor row a = Bits.testBit row $ offsetTable ! a

    numValuations :: Int
    numValuations = Bits.shiftL 1 asLen

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
    fixw s = text $ s ++ replicate (width - length s) '\SP'

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

tautology :: forall a. (Eq a, Ord a) => Formula a -> Bool
tautology fm = getAll . mconcat . onAllValuations subfn $ atoms fm
  where
    subfn :: (a -> Bool) -> All
    subfn = All . eval fm

unsatisfiable :: (Eq a, Ord a) => Formula a -> Bool
unsatisfiable = tautology . FmNot

satisfiable :: (Eq a, Ord a) => Formula a -> Bool
satisfiable = not . unsatisfiable

-- | Substitutes atoms according to a partial function mapping
psubst :: (Ord a, Hashable a) => Function a (Formula a) -> Formula a -> Formula a
psubst f = onAtoms $ join (tryApplyWithDefault f . FmAtom)
