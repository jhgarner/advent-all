module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (take)
import Prelude hiding (take)
import Util.Util
import Data.Monoid (Product(..), Sum(..))
import Data.Semigroup
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Text (Text, unpack, pack)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ do
  row <- unpack <$> take 7
  seat <- unpack <$> take 3
  pure $ ( map (\a -> if a == 'F' then 0 else 1) row
         , map (\a -> if a == 'L' then 0 else 1) seat
         )

------------ TYPES ------------
type Input = [([Int], [Int])]

type OutputA = Int

type OutputB = Maybe (Int, Int)

------------ PART A ------------
toInt :: [Int] -> Int
toInt [] = 0
toInt (l:ls) = l * 2^(length ls) + toInt ls

toSeatScore :: ([Int], [Int]) -> Int
toSeatScore (a, b) = toInt a * 8 + toInt b

partA :: Input -> OutputA
partA = maximum . fmap toSeatScore

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let ls = sort $ fmap toSeatScore input
   in find (\(a, b) -> b - a == 2) $ zip ls (tail ls)
