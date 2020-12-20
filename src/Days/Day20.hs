{-# LANGUAGE PatternSynonyms #-}

module Days.Day20 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (reverse, flip)
-- import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vec
-- import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Control.Applicative
import Data.Massiv.Array hiding (product, tail, take, drop)
import Data.Foldable (find)
import Debug.Trace
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

type OutputB = Array U Ix2 Char

------------ PART A ------------

shareSide' :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
shareSide' a a' =
  sideEqual' (a <! 0) a' ||
  sideEqual' (a <! 9) a' ||
  sideEqual' (a !> 0) a' ||
  sideEqual' (a !> 9) a'
  where
    sideEqual' :: Array M Ix1 Char -> Array U Ix2 Char -> Bool
    sideEqual' side a =
      let options = [a <! 0, a <! 9, a !> 0, a !> 9]
        in side `elem` options

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
partA = error "Disabled to make partB run faster"
-- partA = product . Map.keys . findCorner

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

pickCorner :: Map Int [Maybe Int] -> Int
pickCorner = head . Map.keys . Map.filter ((== 2) . length . filter (not . null))

rotateTL :: [Maybe Int] -> ([Maybe Int], Int)
rotateTL ls@[Nothing, Just _, Just _, Nothing] = (ls, 0)
rotateTL ls = fmap (+1) $ rotateTL $ traceShowId $ take 4 $ drop 3 $ cycle ls

rotateWithLeft :: Int -> [Maybe Int] -> ([Maybe Int], Int)
rotateWithLeft i ls@[top, right, bottom, left]
  | left == Just i = (ls, 0)
  | bottom == Just i = ([left, top, right, bottom], 1)
  | right == Just i = ([bottom, left, top, right], 2)
  | top == Just i = ([right, bottom, left, top], 3)
  
rotateWithTop :: Int -> [Maybe Int] -> ([Maybe Int], Int)
rotateWithTop i ls@[top, right, bottom, left]
  | top == Just i = (ls, 0)
  | left == Just i = ([left, top, right, bottom], 1)
  | bottom == Just i = ([bottom, left, top, right], 2)
  | right == Just i = ([right, bottom, left, top], 3)

fixTopLeft :: Int -> Map Int [Maybe Int] -> (Map Int [Maybe Int], Int)
fixTopLeft i m =
  let (roted, numRot) = rotateTL $ m Map.! i
      fixed = Map.update (Just . const roted) i m
   in (fixed, numRot)

rotateMatrix :: Int -> Array U Ix2 Char -> Array U Ix2 Char
rotateMatrix 0 a = a
rotateMatrix i a = rotateMatrix (i-1) $ computeAs U $ reverse Dim1 $ transpose a

makeRow :: Int -> Map Int [Maybe Int] -> (Map Int [Maybe Int], [(Int, Int)])
makeRow start m =
  let tl = traceShowId $ m Map.! start
      secondColId = fromJust $ tl !! 1
      (newOrder, numRot) = rotateWithLeft start $ m Map.! secondColId
      newM = Map.insert secondColId newOrder m
   in if null $ tl !! 1
      then (m, [])
      else (:) (secondColId, numRot) <$> makeRow secondColId newM

makeRows :: Int -> Map Int [Maybe Int] -> (Map Int [Maybe Int], [[(Int, Int)]])
makeRows start m =
  let (newM, newRow) = makeRow start m
      nextOneId = (newM Map.! start) !! 2
      nextOne = newM Map.! fromJust nextOneId
      (newOrder, numRot) = rotateWithTop start nextOne
      mWithNewOrder = Map.insert (fromJust nextOneId) newOrder newM
      (restM, restLs) = makeRows (fromJust nextOneId) mWithNewOrder
      withStart = ((fromJust nextOneId, numRot) : head restLs) : tail restLs
   in case nextOneId of
        Nothing -> (newM, [newRow])
        Just _ -> (restM, newRow : withStart)

flip :: Map Int (Array U Ix2 Char) -> [[(Int, Int)]] -> Map Int (Array U Ix2 Char)
flip m rows = fst $
  foldl (\(newM, topMaybe) row ->
           (\(newerM, _) -> (newerM, Just $ newerM Map.! fst (head row))) $
           foldl (\(newestM, leftMaybe) (cellI, _) ->
                    let cell = newestM Map.! cellI
                        left = fromJust leftMaybe
                        top = fromJust topMaybe
                        flippedL = rotateMatrix 2 $ compute $ reverse Dim2 cell
                        flippedT = compute $ reverse Dim2 $ rotateMatrix 2 $ compute $ reverse Dim1 cell
                     in case leftMaybe of
                          Just _ -> if shareSide' left cell then (newestM, Just cell) else (Map.insert cellI flippedL newestM, Just flippedL)
                          Nothing -> if null topMaybe || shareSide' top cell then (newestM, Just cell) else (Map.insert cellI flippedT newestM, Just flippedT))
           (newM, Nothing) row)
  (m, Nothing) rows
stitch :: Map Int (Array U Ix2 Char) -> [[(Int, Int)]] -> Array U Ix2 Char
stitch m rows =
  computeAs U $ concat' (Dim 2) $ fmap (computeAs U . concat' (Dim 1) . fmap (\(i, rot) -> rotateMatrix rot $ m Map.! i)) rows

partB :: Input -> OutputB
partB m =
  let adjacent = toAdjacent m
      corner = pickCorner adjacent
      (adjacentFixed, numRot) = fixTopLeft corner adjacent
      (_, rows) = makeRows corner adjacentFixed
      withCorner = ((corner, numRot) : head rows) : tail rows
   in stitch (flip m withCorner) withCorner

-- [[(1171,1),(2473,0),(3079,3)],[(1489,3),(1427,1),(2311,1)],[(2971,3),(2729,1),(1951,1)]]
