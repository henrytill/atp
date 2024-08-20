{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module PropLogic.Syntax where

import Data.Data
import Text.PrettyPrint.HughesPJClass
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
  deriving (Show, Eq, Data, Typeable, Functor)

instance (Pretty a) => Pretty (Formula a) where
  pPrint form =
    case form of
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

newtype Prop = MkProp {unProp :: String}
  deriving (Show, Eq, Data, Typeable)

instance Pretty Prop where
  pPrint = text . unProp
