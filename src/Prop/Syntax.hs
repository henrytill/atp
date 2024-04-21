{-# LANGUAGE DeriveDataTypeable #-}

module Prop.Syntax where

import Data.Data

data Formula a
  = False
  | True
  | Atom a
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  | ForAll String (Formula a)
  | Exists String (Formula a)
  | MetaVar String
  deriving (Show, Eq, Data, Typeable)

newtype Prop = MkProp {unProp :: String}
  deriving (Show, Eq, Data, Typeable)
