module Days.Day03 (runDay) where

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
import qualified Data.Text as T

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1' $ do
  line <- takeTill (== '\n')
  endOfLine
  return $ cycle $ map (== '#') $ T.unpack line
------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA m = direction (0, 0)
  where
    direction :: (Int, Int) -> Int
    direction (x, y)
      | y >= length m = 0
      | m !! y !! x = 1 + direction (x+3, y+1)
      | otherwise = direction (x+3, y+1)


------------ PART B ------------
partB :: Input -> OutputB
partB m = foldl' (*) 1 $ map (direction (0, 0)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    direction :: (Int, Int) -> (Int, Int) -> Int
    direction (x, y) (dx, dy)
      | y >= length m = 0
      | m !! y !! x = 1 + direction (x+dx, y+dy) (dx, dy)
      | otherwise = direction (x+dx, y+dy) (dx, dy)
