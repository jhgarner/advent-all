{-# LANGUAGE PatternSynonyms #-}

module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (replicate, sum, map)
-- import Data.List
import Data.Map.Strict (Map)
import Data.Foldable hiding (sum)
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Massiv.Array
import Debug.Trace
import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ do
  many1' $ choice
    [ char '.' $> 0
    , char '#' $> 1
    ]

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int
-- type OutputA = Array U Ix3 Int

type OutputB = Int

------------ PART A ------------
prepRound :: Input -> Array U Ix3 Int
prepRound ls = fromLists' Par [ls]

-- This is poorly named. It grows the array by 6 since that's the max size
growByOne :: Array U Ix3 Int -> Array U Ix3 Int
growByOne a = computeAs U $ foldl' (\acc i -> let Sz3 d h w = size acc
                                                  newSize =
                                                    case i of
                                                      1 -> Sz3 d h 6
                                                      2 -> Sz3 d 6 w
                                                      3 -> Sz3 6 h w
                                                  border = computeAs U $ replicate Par newSize 0
                                              in computeAs U $ concat' (Dim i) [border, acc, border]) a [1, 2, 3]

actionStencil :: Stencil Ix3 Int Int
actionStencil = makeStencilDef 0 (Sz (3 :> 3 :. 3)) (1 :> 1 :. 1) $ \get ->
  shouldActive <$> myState get <*> sum (fmap (countNeighber get) ((-1, -1, -1) ... (1, 1, 1)))
  where
    countNeighber _ (0, 0, 0) = 0
    countNeighber get (i, j, k) = get (i :> j :. k)
    myState get = get $ 0 :> 0 :. 0
    shouldActive me i
      | me == 1 && (i == 2 || i == 3) = 1
      | i == 3 = 1
      | otherwise = 0

applyStep :: Array U Ix3 Int -> Array U Ix3 Int
applyStep a = computeAs U $ mapStencil (Fill 0) actionStencil a

partA :: Input -> OutputA
partA = sum . applyStep . applyStep . applyStep . applyStep . applyStep . applyStep . growByOne . prepRound

------------ PART B ------------

prepRoundB :: Input -> Array U Ix4 Int
prepRoundB ls = fromLists' Par [[ls]]

-- This is poorly named. It grows the array by 6 since that's the max size
growByOneB :: Array U Ix4 Int -> Array U Ix4 Int
growByOneB a = computeAs U $ foldl' (\acc i -> let Sz4 z d h w = size acc
                                                   newSize =
                                                     case i of
                                                       1 -> Sz4 z d h 6
                                                       2 -> Sz4 z d 6 w
                                                       3 -> Sz4 z 6 h w
                                                       4 -> Sz4 6 d h w
                                                   border = computeAs U $ replicate Par newSize 0
                                                in computeAs U $ concat' (Dim i) [border, acc, border]) a [1, 2, 3, 4]

actionStencilB :: Stencil Ix4 Int Int
actionStencilB = makeStencilDef 0 (Sz (3 :> 3 :> 3 :. 3)) (1 :> 1 :> 1 :. 1) $ \get ->
  shouldActive <$> myState get <*> sum (map (countNeighber get) ((-1, -1, -1, -1) ... (1, 1, 1, 1)))
  where
    countNeighber get (i, j, k, w) = get (i :> j :> k :. w)
    myState get = get $ 0 :> 0 :> 0 :. 0
    shouldActive me i
      | me == 1 && (i == 3 || i == 4) = 1
      | i == 3 = 1
      | otherwise = 0

applyStepB :: Array U Ix4 Int -> Array U Ix4 Int
applyStepB a = computeAs U $ mapStencil (Fill 0) actionStencilB a

partB :: Input -> OutputB
partB = sum . applyStepB . applyStepB . applyStepB . applyStepB . applyStepB . applyStepB . growByOneB . prepRoundB
