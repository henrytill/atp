{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module PropLogic.Syntax where

import Control.Monad
import Data.Data (Data, Typeable)
import Data.Hashable
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJClass (Pretty (..), char, parens, text, (<+>), (<>))
import Prelude hiding ((<>))

data Formula a
  = FmFalse
  | FmTrue
  | FmAtom a
  | FmNot (Formula a)
  | FmAnd (Formula a) (Formula a)
  | FmOr (Formula a) (Formula a)
  | FmImp (Formula a) (Formula a)
  | FmIff (Formula a) (Formula a)
  | FmForAll String (Formula a)
  | FmExists String (Formula a)
  | FmMetaVar String
  deriving (Show, Eq, Data, Typeable, Functor, Generic)

instance (Hashable a) => Hashable (Formula a)

instance (Pretty a) => Pretty (Formula a) where
  pPrint fm =
    case fm of
      FmFalse -> text "false"
      FmTrue -> text "true"
      FmAtom x -> pPrint x
      FmNot x -> parens (char '~' <+> pPrint x)
      FmAnd x y -> binary "/\\" x y
      FmOr x y -> binary "\\/" x y
      FmImp x y -> binary "==>" x y
      FmIff x y -> binary "<=>" x y
      FmForAll {} -> undefined
      FmExists {} -> undefined
      FmMetaVar s -> char '$' <> text s
    where
      binary s x y = parens (pPrint x <+> text s <+> pPrint y)

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

newtype Atoms a = MkAtoms {unAtoms :: Formula a}
  deriving (Show, Eq, Functor)

instance Foldable Atoms where
  foldr f z (MkAtoms fm) =
    case fm of
      FmFalse -> z
      FmTrue -> z
      FmAtom a -> f a z
      FmNot p -> foldr f z (MkAtoms p)
      FmAnd p q -> foldr f (foldr f z (MkAtoms q)) (MkAtoms p)
      FmOr p q -> foldr f (foldr f z (MkAtoms q)) (MkAtoms p)
      FmImp p q -> foldr f (foldr f z (MkAtoms q)) (MkAtoms p)
      FmIff p q -> foldr f (foldr f z (MkAtoms q)) (MkAtoms p)
      FmForAll _ p -> foldr f z (MkAtoms p)
      FmExists _ p -> foldr f z (MkAtoms p)
      FmMetaVar _ -> z

overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms f fm b = foldr f b (MkAtoms fm)

newtype Prop = MkProp {unProp :: String}
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Hashable Prop

instance Pretty Prop where
  pPrint = text . unProp
