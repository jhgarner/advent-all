{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Util.Util where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, intersection)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (fold)
import Data.Semigroup.Foldable
import Data.Bifoldable
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

newtype Intersection a = Intersection (Set a)

instance Ord a => Semigroup (Intersection a) where
  Intersection a <> Intersection b = Intersection $ a `intersection` b


foldN :: forall b a f. (Coercible a b, Coercible (f a) (f b), Monoid b, Foldable f) => (a -> b) -> f a -> a
foldN _ fs = coerce @b @a $ fold $ coerce @(f a) @(f b) fs

fold1N :: forall b a f. (Coercible a b, Coercible (f a) (f b), Semigroup b, Foldable1 f) => (a -> b) -> f a -> a
fold1N _ fs = coerce @b @a $ fold1 $ coerce @(f a) @(f b) fs

foldMapN :: forall b a c f. (Coercible a b, Coercible (f a) (f b), Monoid b, Foldable f) => (a -> b) -> (c -> a) -> f c -> a
foldMapN _ f fs = coerce @b @a $ foldMap (coerce @a @b . f) fs

foldMap1N :: forall b a c f. (Coercible a b, Coercible (f a) (f b), Semigroup b, Foldable1 f) => (a -> b) -> (c -> a) -> f c -> a
foldMap1N _ f fs = coerce @b @a $ foldMap1 (coerce @a @b . f) fs

bifoldN :: forall b a f. (Coercible a b, Coercible (f a a) (f b b), Monoid b, Bifoldable f) => (a -> b) -> f a a -> a
bifoldN _ fs = coerce @b @a $ bifold $ coerce @(f a a) @(f b b) fs


-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : (attachCoords x (y + 1) (ls : lss))

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | length ls < n = [ls]
  | otherwise = (take n ls) : (chunksOf n (drop n ls))
