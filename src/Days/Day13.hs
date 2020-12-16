module Days.Day13 (runDay) where

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
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (decimal <* endOfLine) <*> do
  sepBy1' (decimal <|> (char 'x' >> pure (-1))) (char ',')

------------ TYPES ------------
type Input = (Int, [Int])

type OutputA = Int

type OutputB = (Int, Int)

------------ PART A ------------
findFirst :: Int -> Int -> Int
findFirst _ (-1) = 100000000
findFirst target n
  | n >= target = n
  | otherwise = n + findFirst (target - n) n
partA :: Input -> OutputA
partA (t, ls) =
  let (time, bus) = minimum $ zip (fmap (findFirst t) ls) ls
   in (time - t) * bus

------------ PART B ------------
findWhenOff :: Int -> Int -> Int -> (Int, Int)
findWhenOff a b d = factorOff (25, 13)
  where
    factorOff (af, bf)
      | b * bf - a * af == d = (af, bf)
      | a * af < b * bf = factorOff (af+1, bf)
      | otherwise = factorOff (af, bf+1)
partB :: Input -> OutputB
partB _ = findWhenOff 7 13 1
