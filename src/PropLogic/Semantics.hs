{-# LANGUAGE ScopedTypeVariables #-}

module PropLogic.Semantics where

import Control.Monad
import Data.Bits qualified as Bits
import Data.Foldable (toList)
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
    (_, offsetTable) =
      List.foldl'
        (\(i, m) a -> (pred i, Map.insert a i m))
        (pred asLen, Map.empty)
        as

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
