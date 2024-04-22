{-# LANGUAGE DeriveDataTypeable #-}

module Prop.Syntax where

import Data.Data
import Text.PrettyPrint
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
  deriving (Show, Eq, Data, Typeable)

newtype Prop = MkProp {unProp :: String}
  deriving (Show, Eq, Data, Typeable)

prettyFormula :: Formula Prop -> Doc
prettyFormula expr =
  case expr of
    FmFalse -> text "false"
    FmTrue -> text "true"
    FmAtom x -> text (unProp x)
    FmNot x -> parens (char '~' <+> prettyFormula x)
    FmAnd x y -> binary "/\\" x y
    FmOr x y -> binary "\\/" x y
    FmImp x y -> binary "==>" x y
    FmIff x y -> binary "<=>" x y
    FmForAll {} -> undefined
    FmExists {} -> undefined
    FmMetaVar s -> char '$' <> text s
  where
    binary s x y = parens (prettyFormula x <+> text s <+> prettyFormula y)
