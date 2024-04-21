{-# LANGUAGE DeriveDataTypeable #-}

module Prop.Syntax where

import Data.Data

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
