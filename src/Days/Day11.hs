module Days.Day11 (runDay) where

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
import Debug.Trace

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
data Cell = Empty | Occupied | Floor | Border
  deriving (Show, Eq)

inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ many1' $ choice
  [ char '.' >> pure Floor
  , char '#' >> pure Occupied
  , char 'L' >> pure Empty
  ]

------------ TYPES ------------
type Input = [[Cell]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
makeSafe :: [[Cell]] -> [[Cell]]
makeSafe ls = fmap (\xs -> Border : xs ++ [Border]) $ fmap (const Border) (head ls) : ls ++ [fmap (const Border) (head ls)]
processCell :: [[Cell]] -> (Int, Int) -> Cell
processCell ls (x, y)
  | anyCellOcc == 0 && ls !! y !! x == Empty = Occupied
  | anyCellOcc >= 4 && ls !! y !! x == Occupied = Empty
  | otherwise = ls !! y !! x
    where
        isOcc Occupied = True
        isOcc _ = False
        cellOcc (x', y')
          | x' == x && y' == y = False
          | otherwise = isOcc $ (makeSafe ls) !! (y'+1) !! (x'+1)
        anyCellOcc = sum $ fmap (\x' -> sum $ fmap (\y' -> fromEnum $ cellOcc (x+x',y+y')) [-1, 0, 1]) [-1, 0, 1]
fixPoint :: [[Cell]] -> [[Cell]]
fixPoint ls = let r = fmap (\y -> fmap (\x -> processCell ls (x, y)) [0..length (ls !! 0)-1]) [0..length ls - 1]
               in if r == ls then r else fixPoint r
numOcc :: [[Cell]] -> Int
numOcc ls = sum $ fmap (\y -> sum $ fmap (\x -> fromEnum (ls !! y !! x == Occupied)) [0..length (ls !! 0)-1]) [0..length ls-1]
partA :: Input -> OutputA
partA = numOcc . fixPoint

------------ PART B ------------
ray :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Bool
ray ls (0, 0) (x, y)
  | ls !! y !! x == Floor = error "Floor"
  | ls !! y !! x == Border = error "Border"
  | otherwise = False
ray ls (dx, dy) (x, y)
  | ls !! y !! x == Occupied = True
  | ls !! y !! x == Floor = ray ls (dx, dy) (x+dx, y+dy)
  | otherwise = False
processCellB :: [[Cell]] -> (Int, Int) -> Cell
processCellB ls (x, y)
  | ls !! y !! x == Empty && anyCellOcc == 0 = Occupied
  | ls !! y !! x == Occupied && anyCellOcc >= 5 = Empty
  | otherwise = ls !! y !! x
    where
        anyCellOcc = sum $ fmap (\x' -> sum $ fmap (\y' -> fromEnum $ ray (makeSafe ls) (x', y') (x+1+x',y+1+y')) [-1, 0, 1]) [-1, 0, 1]
fixPointB :: [[Cell]] -> [[Cell]]
fixPointB ls = let r = fmap (\y -> fmap (\x -> processCellB ls (x, y)) [0..length (ls !! 0)-1]) [0..length ls - 1]
               in if r == ls then r else fixPointB r
partB :: Input -> OutputB
partB = numOcc . fixPointB
