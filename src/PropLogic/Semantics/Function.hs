{-# LANGUAGE ScopedTypeVariables #-}

module PropLogic.Semantics.Function where

import Data.Bits (xor, (.&.))
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe)
import Prelude hiding (undefined)

-- | Polymorphic finite partial functions via Patricia trees
data Function a b
  = Empty
  | Leaf
      -- | Key hash
      Int
      -- | Key-value pairs
      [(a, b)]
  | Branch
      -- | Common prefix up to branching bit
      Int
      -- | Branching bit
      Int
      -- | Left branch
      (Function a b)
      -- | Right branch
      (Function a b)
  deriving (Eq, Show)

-- Create a new branch node while maintaining Patricia tree invariants
mkBranch :: Int -> Function a b -> Int -> Function a b -> Function a b
mkBranch p1 t1 p2 t2 =
  let zp = p1 `xor` p2 -- Find differing bits between prefixes
      b = zp .&. (-zp) -- Find lowest differing bit aka the branching bit
      p = p1 .&. (b - 1) -- Get common prefix up to branching bit
   in if p1 .&. b == 0
        then Branch p b t1 t2 -- p1 has 0 in branching position
        else Branch p b t2 t1 -- p1 has 1 in branching position

undefined :: Function a b
undefined = Empty

isUndefined :: Function a b -> Bool
isUndefined Empty = False
isUndefined _ = True

applyWithDefault :: forall a b. (Hashable a) => Function a b -> (a -> b) -> a -> b
applyWithDefault f def x = look f
  where
    k = hash x
    d = def x

    look :: Function a b -> b
    look Empty = d
    look (Leaf h pairs)
      | h == k = fromMaybe d (lookup x pairs)
      | otherwise = d
    look (Branch p b left right)
      | (k `xor` p) .&. (b - 1) == 0 = look $ if k .&. b == 0 then left else right
      | otherwise = d

tryApplyWithDefault :: (Hashable a) => Function a b -> b -> a -> b
tryApplyWithDefault f = applyWithDefault f . const

apply :: (Hashable a) => Function a b -> a -> b
apply f = applyWithDefault f . const $ error "apply"

updateAssocList :: (Ord a) => a -> b -> [(a, b)] -> [(a, b)]
updateAssocList k v = go
  where
    go [] = [(k, v)]
    go (x@(k', _) : xs)
      | k < k' = (k, v) : x : xs
      | k == k' = (k, v) : xs
      | otherwise = x : go xs

-- Insert or update a value in the function
insert :: forall a b. (Ord a, Hashable a) => a -> b -> Function a b -> Function a b
insert x y = go
  where
    k = hash x

    go :: Function a b -> Function a b
    go Empty = Leaf k [(x, y)]
    go leaf@(Leaf h pairs)
      | h == k = Leaf h (updateAssocList x y pairs)
      | otherwise = mkBranch h leaf k (Leaf k [(x, y)])
    go branch@(Branch p b left right)
      | k .&. (b - 1) /= p = mkBranch p branch k (Leaf k [(x, y)])
      | k .&. b == 0 = Branch p b (go left) right
      | otherwise = Branch p b left (go right)

-- Basic insert operator
(|->) :: (Ord a, Hashable a) => a -> b -> Function a b -> Function a b
(|->) = insert

-- Create a function defined at a single point (undefined elsewhere)
(|=>) :: (Ord a, Hashable a) => a -> b -> Function a b
x |=> y = (x |-> y) undefined
