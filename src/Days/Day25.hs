module Days.Day25 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> decimal <*> (endOfLine *> decimal)

------------ TYPES ------------
type Input = (Int, Int)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
guessAll :: Int -> Int -> Int -> Int
guessAll n pk goalPk =
  let newPk = (pk * 7) `mod` 20201227
   in if newPk == goalPk then n else guessAll (n+1) newPk goalPk

genIt :: Int -> Int -> Int -> Int
genIt 0 pk i = i
genIt n pk i = genIt (n-1) pk $ (pk * i) `mod` 20201227

partA :: Input -> OutputA
partA (a, b) = genIt (guessAll 1 1 a) b 1

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
