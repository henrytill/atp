{-# LANGUAGE DeriveDataTypeable #-}

module Intro.Syntax where

import Data.Data
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))

data Expression
  = Var String
  | Const Integer
  | Neg Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Exp Expression Expression
  | MetaVar String
  deriving (Show, Eq, Data, Typeable)

instance Pretty Expression where
  pPrint expr =
    case expr of
      Var s -> text s
      Const i -> integer i
      Neg x -> parens (char '-' <+> pPrint x)
      Add x y -> binary '+' x y
      Sub x y -> binary '-' x y
      Mul x y -> binary '*' x y
      Exp x y -> binary '^' x y
      MetaVar s -> char '$' <> text s
    where
      binary c x y = parens (pPrint x <+> char c <+> pPrint y)
