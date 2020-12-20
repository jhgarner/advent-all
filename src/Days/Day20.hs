{-# LANGUAGE PatternSynonyms #-}

module Days.Day20 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (reverse)
-- import Data.List
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
import Data.Massiv.Array hiding (product)
import Data.Foldable (find)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = toArrays . Map.fromList <$> tiles
  where
    tileName = string "Tile " >> (decimal <* string ":\n")
    image = sepBy1' (many1' $ char '#' <|> char '.') endOfLine
    tile = (,) <$> tileName <*> image
    tiles = sepBy1' tile $ endOfLine >> endOfLine

------------ TYPES ------------
toArrays :: Map Int [[Char]] -> Map Int (Array U Ix2 Char)
toArrays = fmap $ fromLists' Par

type Input = Map Int (Array U Ix2 Char)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
findCorner :: Input -> Input
findCorner m = Map.filter check m
  where
    check a = length (Map.filter (shareSide a) m) == 3
                          
    shareSide :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
    shareSide a a' =
      sideEqual (a <! 0) a' ||
      sideEqual (a <! 9) a' ||
      sideEqual (a !> 0) a' ||
      sideEqual (a !> 9) a'
    sideEqual :: Array M Ix1 Char -> Array U Ix2 Char -> Bool
    sideEqual side a =
      let options = [a <! 0, a <! 9, a !> 0, a !> 9]
       in side `elem` options || toManifest (computeAs U (reverse Dim1 side)) `elem` options

partA :: Input -> OutputA
partA = product . Map.keys . findCorner

------------ PART B ------------
toAdjacent :: Input -> Map Int [Maybe Int]
toAdjacent m = Map.mapWithKey findSides m
  where
    findSides i a = fmap fst <$> fmap (\f -> find (f a . snd) $ Map.toList $ Map.delete i m) [shareSideTop, shareSideRight, shareSideBottom, shareSideLeft]

    shareSideTop :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
    shareSideTop a a' = sideEqual (a !> 0) a'
    shareSideRight :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
    shareSideRight a a' = sideEqual (a <! 9) a'
    shareSideBottom :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
    shareSideBottom a a' = sideEqual (a !> 9) a'
    shareSideLeft :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
    shareSideLeft a a' = sideEqual (a <! 0) a'
    sideEqual :: Array M Ix1 Char -> Array U Ix2 Char -> Bool
    sideEqual side a =
      let options = [a <! 0, a <! 9, a !> 0, a !> 9]
       in side `elem` options || toManifest (computeAs U (reverse Dim1 side)) `elem` options

-- pickCorner :: Map Int [Maybe Int] -> Int
-- pickCorner = head . Map.keys . Map.filter ((== 3) . length . filter (not . null))
  -- where
    

-- makeRow :: Int -> Map Int [Int] -> [Int]
-- makeRow start m =
--   let 

partB :: Input -> OutputB
partB = error "Not implemented yet!"
