{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ApplicativeDo #-}

module Days.Day20 (runDay) where

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- WARNING: This code is trash and doesn't work. I modified the input to appease
-- the code a little bit but rotations/flips are absolutely destroying my brain.
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

{- ORMOLU_DISABLE -}
import Prelude hiding (reverse, map, sequenceA, zip, or, all, sum, any)
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
-- import Data.Void
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

type OutputB = Int
-- type OutputB = Array U Ix2 Char

------------ PART A ------------

shareSide' :: Array U Ix2 Char -> Array U Ix2 Char -> Bool
shareSide' a a' =
  sideEqual' False (a <! 0) a' ||
  sideEqual' True (a <! 9) a' ||
  sideEqual' True (a !> 0) a' ||
  sideEqual' False (a !> 9) a'
  where
    sideEqual' :: Bool -> Array M Ix1 Char -> Array U Ix2 Char -> Bool
    sideEqual' True side a =
      let options = fmap toManifest [computeAs U $ a <! 0, computeAs U $ reverse Dim1 $ a <! 9, computeAs U $ reverse Dim1 $ a !> 0, compute $ a !> 9]
        in side `elem` options
    sideEqual' False side a =
      let options = fmap toManifest [computeAs U $ reverse Dim1 $ a <! 0, computeAs U $ a <! 9, computeAs U $ a !> 0, compute $ reverse Dim1 $ a !> 9]
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

rotateTL :: [Maybe Int] -> [Maybe Int]
rotateTL ls@[Nothing, Just _, Just _, Nothing] = ls
rotateTL ls = rotateTL $ take 4 $ drop 3 $ cycle ls

rotateWithLeft :: Int -> [Maybe Int] -> [Maybe Int]
rotateWithLeft i ls@[top, right, bottom, left]
  | left == Just i = ls
  | bottom == Just i = [left, top, right, bottom]
  | right == Just i = [bottom, left, top, right]
  | top == Just i = [right, bottom, left, top]
  
rotateWithTop :: Int -> [Maybe Int] -> [Maybe Int]
rotateWithTop i ls@[top, right, bottom, left]
  | top == Just i = ls
  | left == Just i = [left, top, right, bottom]
  | bottom == Just i = [bottom, left, top, right]
  | right == Just i = [right, bottom, left, top]

fixTopLeft :: Int -> Map Int [Maybe Int] -> Map Int [Maybe Int]
fixTopLeft i m =
  let roted = rotateTL $ m Map.! i
      fixed = Map.update (Just . const roted) i m
   in fixed

rotateMatrix :: Int -> Array U Ix2 Char -> Array U Ix2 Char
rotateMatrix 0 a = a
rotateMatrix i a = rotateMatrix (i-1) $ computeAs U $ reverse Dim1 $ transpose a

makeRow :: Int -> Map Int [Maybe Int] -> (Map Int [Maybe Int], [Int])
makeRow start m =
  let tl = m Map.! start
      secondColId = fromJust (tl !! 1)
      newOrder = rotateWithLeft start $ m Map.! secondColId
      newM = Map.insert secondColId newOrder m
   in if null $ tl !! 1 
      then (m, [])
      else (:) secondColId <$> makeRow secondColId newM

makeRows :: Int -> Map Int [Maybe Int] -> (Map Int [Maybe Int], [[Int]])
makeRows start m =
  let (newM, newRow) = makeRow start m
      nextOneId = (newM Map.! start) !! 2
      nextOne = newM Map.! fromJust nextOneId
      newOrder = rotateWithTop start nextOne
      mWithNewOrder = Map.insert (fromJust nextOneId) newOrder newM
      (restM, restLs) = makeRows (fromJust nextOneId) mWithNewOrder
   in case nextOneId of
        Nothing -> (newM, [start : newRow])
        Just _ -> (restM, (start : newRow) : restLs)

flipIt :: Map Int (Array U Ix2 Char) -> [[(Int, Int)]] -> Map Int (Array U Ix2 Char)
flipIt m rows = fst $
  foldl (\(newM, topMaybe) row ->
           (\(newerM, _) -> (newerM, Just $ newerM Map.! fst (head row))) $
           foldl (\(newestM, leftMaybe) (cellI, _) ->
                    let cell = newestM Map.! cellI
                        left = fromJust leftMaybe
                        top = fromJust topMaybe
                        flippedL = compute $ reverse Dim2 cell
                        flippedT = compute $ reverse Dim1 cell
                     in case leftMaybe of
                          Just _ -> if shareSide' left cell then (newestM, Just cell) else (Map.insert cellI flippedL newestM, Just flippedL)
                          Nothing -> if null topMaybe || shareSide' top cell then (newestM, Just cell) else (Map.insert cellI flippedT newestM, Just flippedT))
           (newM, Nothing) row)
  (m, Nothing) rows
stitch :: Map Int (Array U Ix2 Char) -> [[(Int, Int)]] -> Array U Ix2 Char
stitch m rows =
  computeAs U $ concat' (Dim 2) $ fmap (computeAs U . concat' (Dim 1) . fmap (\(i, rot) -> extract' (Ix2 0 0) (Sz2 10 10) $ m Map.! i)) rows
  -- computeAs U $ concat' (Dim 2) $ fmap (computeAs U . concat' (Dim 1) . fmap (\(i, rot) -> extract' (Ix2 0 0) (Sz2 10 10) $ rotateMatrix rot $ m Map.! i)) rows

countMonster :: Stencil Ix2 Char Bool
countMonster = makeStencilDef '.' (Sz2 3 20) (Ix2 0 0) $ \get ->
  let zipped = zip seaMonster <$> sequenceA @U (map get $ Ix2 0 0 ... Ix2 2 19)
   in all isValid <$> zipped
  where
    isValid ('.', _) = True
    isValid ('#', '#') = True
    isValid _ = False
    seaMonster :: Array U Ix2 Char
    seaMonster = fromLists' Par [['.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','#','.'],['#','.','.','.','.','#','#','.','.','.','.','#','#','.','.','.','.','#','#','#'],['.','#','.','.','#','.','.','#','.','.','#','.','.','#','.','.','#','.','.','.']]

rotateUntil :: Int -> [Maybe Int] -> [Maybe Int] -> Maybe Int
rotateUntil 0 _ _ = Nothing
rotateUntil i as bs
  | as == bs = Just 0
  | otherwise = rotateUntil (i-1) (take 4 $ drop 3 $ cycle as) bs

rotateStencil :: Map Int [Maybe Int] -> Stencil Ix2 (Maybe Int) Int
rotateStencil m = makeStencilDef Nothing (Sz2 3 3) (Ix2 1 1) $ \get -> do
  directions <- traverse get [Ix2 0 1, Ix2 1 0, Ix2 0 (-1), Ix2 (-1) 0]
  me <- get $ Ix2 0 0
  
  pure $ if null me then 0 else fromMaybe (error $ show directions ++ " " ++ show me ++ show (m Map.! fromJust me)) $ rotateUntil 4 (m Map.! fromJust me) directions <|> rotateUntil 4 (flipOrder $ m Map.! fromJust me) directions
  where flipOrder [a, b, c, d] = [c, b, a, d]

countWave :: Char -> Int
countWave '#' = 1
countWave _ = 0

applyRotation :: Map Int (Array U Ix2 Char) -> Array D Ix2 (Int, Int) -> Map Int (Array U Ix2 Char)
applyRotation = foldl (\newM (key, rot) -> Map.update (Just . rotateMatrix rot) key newM)

partB :: Input -> OutputB
partB m =
  let adjacent = toAdjacent m
      corner = pickCorner adjacent
      adjacentFixed = fixTopLeft corner adjacent
      (_, rows) = makeRows corner adjacentFixed
      withCorner = traceShowId rows
      withCornerZip = zip (fromLists' @N Par withCorner) $ computeAs N $ mapStencil (Fill Nothing) (rotateStencil adjacentFixed) $traceShowId $ computeAs N $ map Just $ fromLists' @U Par withCorner
      rotated = applyRotation m withCornerZip
      andFlipped = flipIt rotated $ toLists2 withCornerZip
      stitched = traceShowId $ stitch andFlipped $ toLists2 withCornerZip
      bestOne = fromJust $ find (or . computeAs U . applyStencil noPadding countMonster) $ fmap (`rotateMatrix` stitched) [0, 1, 2, 3]
      numMonsters = sum $ map fromEnum $ computeAs U $ applyStencil noPadding countMonster bestOne
   -- in stitched
   -- in sum (map countWave stitched) -- - numMonsters * 15
   in sum (map countWave bestOne) - numMonsters * 15

-- [[(1171,1),(2473,0),(3079,3)],[(1489,3),(1427,1),(2311,1)],[(2971,3),(2729,1),(1951,1)]]
