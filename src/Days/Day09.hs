{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
{- ORMOLU_ENABLE -}

data Btree a = Leaf | Branch a (Btree a) (Btree a)
makeBaseFunctor ''Btree

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' decimal endOfLine

------------ TYPES ------------
type Input = [Integer]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
p :: Int
p = 25

sumTo :: Integer -> [Integer] -> Bool
sumTo n = para sumTo'
  where
    sumTo' (Cons x (xs, xsSumsToN)) = elem (n-x) xs || xsSumsToN
    sumTo' Nil = False

partA :: Input -> OutputA
partA = para partA'
  where
    partA' (Cons x (xs, resultFromXs)) =
      if sumTo (xs !! (p-1)) $ take p $ x:xs
      then resultFromXs
      else xs !! (p-1)
    partA' Nil = error "No flaw in encryption"

------------ PART B ------------

type Window = (Int, Int, Integer)

contSum :: Integer -> Vector Integer -> Vector Integer
contSum n v = Vec.slice start len v
        -- Because of laziness and fusion, Haskell won't actually construct an
        -- intermediate Btree.
  where (start, len, _) = hylo findSum allWindows (0, 0, Vec.head v)

        -- Generates all possible sliding windows.
        -- Yay for laziness!
        allWindows :: Window -> BtreeF Window Window
        allWindows w@(start, len, s)
          | len < 0 = LeafF
          | otherwise = BranchF w lenUp startUp
            where
              lenUp = (start, len+1, s + (v ! (start+len+1)))
              startUp = (start+1, len-1, s - (v ! start))

        -- Travels along one branch searching for a valid sliding window.
        findSum :: BtreeF Window Window -> Window
        findSum LeafF = error "Not found"
        findSum (BranchF w@(_, _, s) lenUp startUp)
          | s < n = lenUp
          | s == n = w
          | s > n = startUp


partB :: Input -> OutputB
partB ls =
  let result = contSum (partA ls) $ Vec.fromList ls
   in minimum result + maximum result
