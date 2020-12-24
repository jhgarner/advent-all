{-# LANGUAGE ApplicativeDo #-}

module Days.Day24 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (zip, map, sum)
import qualified Prelude as P
import Data.List hiding (zip, map, sum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Massiv.Array

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ many1' $ choice
  [ string "e" $> (1, 0)
  , string "w" $> (-1, 0)
  , string "nw" $> (0, -1)
  , string "se" $> (0, 1)
  , string "ne" $> (1, -1)
  , string "sw" $> (-1, 1)
  ]

------------ TYPES ------------
type Input = [[(Int, Int)]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

follow :: [(Int, Int)] -> (Int, Int)
follow = foldl' (\(x, y) (dx, dy) -> (x+dx, y+dy)) (0, 0)

toCount :: [(Int, Int)] -> Map (Int, Int) Int
toCount = Map.fromListWith (+) . flip P.zip (repeat 1)

toArray :: [(Int, Int)] -> Array U Ix2 Bool
toArray ls = makeArray Par (Sz2 200 200) $
  \(Ix2 y x) -> (x - 100, y - 100) `elem` ls

rule :: Stencil Ix2 Bool Bool
rule = makeStencilDef False (Sz2 3 3) (Ix2 1 1) $ \get -> do
  me <- get $ Ix2 0 0
  neighbors <- sum . computeAs U . map fromEnum <$> traverseA @U get (fromList @B Par [Ix2 1 (-1), Ix2 (-1) 1, Ix2 0 1, Ix2 0 (-1), Ix2 1 0, Ix2 (-1) 0])
  pure $ if
    | me && (neighbors == 0 || neighbors > 2) -> False
    | not me && neighbors == 2 -> True
    | otherwise -> me

partA :: Input -> OutputA
partA = length . Map.filter odd . traceShowId . toCount . fmap follow

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map fromEnum . applyN 100 . toArray . Map.keys . Map.filter odd . toCount . fmap follow
  where applyN 0 = id
        applyN n = applyN (n-1) . computeAs U . mapStencil @U (Fill False) rule
  
