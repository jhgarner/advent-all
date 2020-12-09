module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)
import Data.Void
{- ORMOLU_ENABLE -}

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
sumTo n (x:xs) = elem (n-x) xs || sumTo n xs
sumTo _ [] = False

partA :: Input -> OutputA
partA ls =
  let pre = take p ls
      isGood = sumTo (ls !! p) pre
   in if isGood then partA (tail ls) else ls !! p

------------ PART B ------------
contSum :: Integer -> [Integer] -> [Integer] -> [Integer]
contSum n hs ts = let s = sum hs in
  if
    | s == n -> hs
    | s > n -> contSum n (tail hs) ts
    | s < n -> contSum n (hs ++ [head ts]) (tail ts)


partB :: Input -> OutputB
partB ls =
  let result = contSum (partA ls) [] ls
   in minimum result + maximum result
