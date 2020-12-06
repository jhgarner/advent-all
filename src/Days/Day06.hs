module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (intersect)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set, fromList, intersection)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Monoid

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ do
  sepBy (many1' letter) endOfLine

------------ TYPES ------------
type Input = [[[Char]]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
groupToSet :: [[Char]] -> Set Char
groupToSet = foldMap fromList

partA :: Input -> OutputA
partA = getSum . foldMap (Sum . length . groupToSet)

------------ PART B ------------
groupToSetI :: [[Char]] -> Set Char
groupToSetI [] = mempty
groupToSetI (l:ls) = foldl' (\acc a -> acc `intersection` fromList a) (fromList l) ls 

partB :: Input -> OutputB
partB = getSum . foldMap (Sum . length . groupToSetI)
