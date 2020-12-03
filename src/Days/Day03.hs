{-# LANGUAGE TypeApplications #-}

module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Util.Util
import Data.Text (unpack)
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Functor.Foldable
import Data.Monoid (Product(..), Sum(..))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1' $ do
  line <- takeTill (== '\n')
  endOfLine
  return $ cycle $ map (== '#') $ unpack line

------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
trajectory :: Input -> (Int, Int) -> Int
trajectory m (dx, dy) = hylo (bifoldN Sum) stepUp (0, 0)
  where
    stepUp :: (Int, Int) -> ListF Int (Int, Int)
    stepUp (x, y)
        | y >= length m = Nil
        | otherwise = fromEnum (m !! y !! x) `Cons` (x+dx,y+dy)

partA :: Input -> OutputA
partA m = trajectory m (3, 1)

------------ PART B ------------
partB :: Input -> OutputB
partB m = foldN Product $ map (trajectory m) paths
  where
    paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
