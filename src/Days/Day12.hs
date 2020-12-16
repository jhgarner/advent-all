module Days.Day12 (runDay) where

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
import Control.Monad.State.Strict

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

data InType = N | S | E | W | L | R | F
  deriving (Show, Eq)
data Instruction = In InType Int
  deriving (Show, Eq)

data Boat = Boat (Int, Int) Int
  deriving (Eq, Show)

data BoatB = BoatB (Int, Int) (Int, Int)
  deriving (Eq, Show)

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' (In <$> inType <*> decimal) endOfLine
  where
    inType = choice
      [ char 'N' >> pure N
      , char 'S' >> pure S
      , char 'E' >> pure E
      , char 'W' >> pure W
      , char 'L' >> pure L
      , char 'R' >> pure R
      , char 'F' >> pure F
      ]
           

------------ TYPES ------------
type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
boatDistance :: Boat -> Int
boatDistance (Boat (x, y) _) = abs x + abs y

followIn :: Instruction -> Boat -> Boat
followIn (In N n) (Boat (x, y) r) = Boat (x, y+n) r
followIn (In S n) (Boat (x, y) r) = Boat (x, y-n) r
followIn (In E n) (Boat (x, y) r) = Boat (x+n, y) r
followIn (In W n) (Boat (x, y) r) = Boat (x-n, y) r
followIn (In R n) (Boat (x, y) r) = Boat (x, y) ((r-n) `mod` 360)
followIn (In L n) (Boat (x, y) r) = Boat (x, y) ((r+n) `mod` 360)
followIn (In F n) b@(Boat _ 0) = followIn (In E n) b
followIn (In F n) b@(Boat _ 90) = followIn (In N n) b
followIn (In F n) b@(Boat _ 180) = followIn (In W n) b
followIn (In F n) b@(Boat _ 270) = followIn (In S n) b

partA :: Input -> OutputA
partA = boatDistance . foldl' (flip followIn) (Boat (0, 0) 0)

------------ PART B ------------
boatBDistance :: BoatB -> Int
boatBDistance (BoatB (x, y) _) = abs x + abs y

followInB :: Instruction -> BoatB -> BoatB
followInB (In N n) (BoatB l (x, y)) = BoatB l (x, y+n)
followInB (In S n) (BoatB l (x, y)) = BoatB l (x, y-n)
followInB (In E n) (BoatB l (x, y)) = BoatB l (x+n, y)
followInB (In W n) (BoatB l (x, y)) = BoatB l (x-n, y)
followInB (In L 0) b = b
followInB (In L n) (BoatB l (dx, dy)) = followInB (In L $ n-90) $ BoatB l (-dy, dx)
followInB (In R 0) b = b
followInB (In R n) (BoatB l (dx, dy)) = followInB (In R $ n-90) $ BoatB l (dy, -dx)
followInB (In F 0) b = b
followInB (In F n) (BoatB (x, y) (dx, dy)) = followInB (In F $ n-1) $ BoatB (x+dx, y+dy) (dx, dy)
  
partB :: Input -> OutputB
partB = boatBDistance . foldl' (flip followInB) (BoatB (0, 0) (10, 1))
