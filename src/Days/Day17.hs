{-# LANGUAGE PatternSynonyms #-}

module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (replicate, sum, map)
-- import Data.List
import Data.Map.Strict (Map)
import Data.Foldable hiding (sum)
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
    [ char '.' $> False
    , char '#' $> True
    ]

------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int
-- type OutputA = Array U Ix3 Bool

type OutputB = Int

------------ PART A ------------
prepRound :: Input -> Array U Ix3 Bool
prepRound ls = fromLists' Par [ls]

growByOne :: Array U Ix3 Bool -> Array U Ix3 Bool
growByOne a = computeAs U $ foldl' (\acc i -> let Sz3 d h w = size acc
                                                  newSize =
                                                    case i of
                                                      1 -> Sz3 d h 6
                                                      2 -> Sz3 d 6 w
                                                      3 -> Sz3 6 h w
                                                  border = computeAs U $ replicate Par newSize False
                                              in concat' (Dim i) [border, computeAs U acc, border]) (toLoadArray a) [1, 2, 3]

actionStencil :: Stencil Ix3 Bool Bool
actionStencil = makeStencilDef False (Sz (3 :> 3 :. 3)) (1 :> 1 :. 1) $ \get ->
  shouldActive <$> myState get <*> sum (fmap (countNeighber get) ((-1, -1, -1) ... (1, 1, 1)))
  where
    countNeighber _ (0, 0, 0) = 0
    countNeighber get (i, j, k) = fromEnum <$> get (i :> j :. k)
    myState get = get $ 0 :> 0 :. 0
    shouldActive me i
      | me && (i == 2 || i == 3) = True
      | i == 3 = True
      | otherwise = False

applyStep :: Array U Ix3 Bool -> Array U Ix3 Bool
applyStep a = convertAs U $ mapStencil (Fill False) actionStencil a

partA :: Input -> OutputA
partA = sum . map fromEnum . applyStep . applyStep . applyStep . applyStep . applyStep . applyStep . growByOne . prepRound

------------ PART B ------------

prepRoundB :: Input -> Array U Ix4 Bool
prepRoundB ls = fromLists' Par [[ls]]

growByOneB :: Array U Ix4 Bool -> Array U Ix4 Bool
growByOneB a = computeAs U $ foldl' (\acc i -> let Sz4 z d h w = size acc
                                                   newSize =
                                                     case i of
                                                       1 -> Sz4 z d h 6
                                                       2 -> Sz4 z d 6 w
                                                       3 -> Sz4 z 6 h w
                                                       4 -> Sz4 6 d h w
                                                   border = computeAs U $ replicate Par newSize False
                                                in concat' (Dim i) [border, computeAs U acc, border]) (toLoadArray a) [1, 2, 3, 4]

actionStencilB :: Stencil Ix4 Bool Bool
actionStencilB = makeStencilDef False (Sz (3 :> 3 :> 3 :. 3)) (1 :> 1 :> 1 :. 1) $ \get ->
  shouldActive <$> myState get <*> sum (fmap (countNeighber get) ((-1, -1, -1, -1) ... (1, 1, 1, 1)))
  where
    countNeighber _ (0, 0, 0, 0) = 0
    countNeighber get (i, j, k, w) = fromEnum <$> get (i :> j :> k :. w)
    myState get = get $ 0 :> 0 :> 0 :. 0
    shouldActive me i
      | me && (i == 2 || i == 3) = True
      | i == 3 = True
      | otherwise = False

applyStepB :: Array U Ix4 Bool -> Array U Ix4 Bool
applyStepB a = convertAs U $ mapStencil (Fill False) actionStencilB a

partB :: Input -> OutputB
partB = sum . map (fromEnum) . applyStepB . applyStepB . applyStepB . applyStepB . applyStepB . applyStepB . growByOneB . prepRoundB
