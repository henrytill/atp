module PropLogic.Semantics.Func where

import Data.Bits
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe)
import Prelude hiding (undefined)

-- Polymorphic finite partial functions via Patricia trees
data Func a b
  = Empty
  | Leaf Int [(a, b)]
  | Branch Int Int (Func a b) (Func a b)
  deriving (Eq, Show)

-- Create a new branch node while maintaining Patricia tree invariants
mkBranch :: Int -> Func a b -> Int -> Func a b -> Func a b
mkBranch p1 t1 p2 t2 =
  let zp = p1 `xor` p2 -- Find differing bits between prefixes
      b = zp .&. (-zp) -- Find lowest differing bit aka the branching bit
      p = p1 .&. (b - 1) -- Get common prefix up to branching bit
   in if p1 .&. b == 0
        then Branch p b t1 t2 -- p1 has 0 in branching position
        else Branch p b t2 t1 -- p1 has 1 in branching position

undefined :: Func a b
undefined = Empty

isUndefined :: Func a b -> Bool
isUndefined Empty = False
isUndefined _ = True

applyWithDefault :: (Hashable a) => Func a b -> (a -> b) -> a -> b
applyWithDefault f def x = look f
  where
    k = hash x -- Get hash of the key
    look Empty = def x
    look (Leaf h pairs)
      | h == k = fromMaybe (def x) (lookup x pairs)
      | otherwise = def x
    look (Branch p b left right)
      | (k `xor` p) .&. (b - 1) == 0 = look (if k .&. b == 0 then left else right)
      | otherwise = def x

tryApplyWithDefault :: (Hashable a) => Func a b -> a -> b -> b
tryApplyWithDefault f a def = applyWithDefault f (const def) a

apply :: (Hashable a) => Func a b -> a -> b
apply f = applyWithDefault f (\_ -> error "apply")

updateAssocList :: (Ord k) => k -> v -> [(k, v)] -> [(k, v)]
updateAssocList k v [] = [(k, v)]
updateAssocList k v pairs@((k', v') : rest) =
  case compare k k' of
    LT -> (k, v) : pairs
    EQ -> (k, v) : rest
    GT -> (k', v') : updateAssocList k v rest

-- Insert or update a value in the function
insert :: (Ord k, Hashable k) => k -> v -> Func k v -> Func k v
insert x y = update (hash x)
  where
    update k Empty = Leaf k [(x, y)]
    update k leaf@(Leaf h pairs)
      | h == k = Leaf h (updateAssocList x y pairs)
      | otherwise = mkBranch h leaf k (Leaf k [(x, y)])
    update k branch@(Branch p b left right)
      | k .&. (b - 1) /= p = mkBranch p branch k (Leaf k [(x, y)])
      | k .&. b == 0 = Branch p b (update k left) right
      | otherwise = Branch p b left (update k right)

-- Basic insert operator
(|->) :: (Ord k, Hashable k) => k -> v -> Func k v -> Func k v
(|->) = insert

-- Create a function defined at a single point (undefined elsewhere)
(|=>) :: (Ord k, Hashable k) => k -> v -> Func k v
x |=> y = (x |-> y) undefined
