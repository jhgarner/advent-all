{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day15 (runDay) where

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
import Control.Monad.State
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Debug.Trace
{- ORMOLU_ENABLE -}

data Beam a = End a | Continue (Beam a)
  deriving (Functor, Show, Eq)
makeBaseFunctor ''Beam

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' decimal (char ',')

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Void



------------ PART A ------------
extractBeam :: BeamF a a -> a
extractBeam (EndF a) = a
extractBeam (ContinueF b) = b

nextNum :: (Map Int Int, Int, Int) -> BeamF Int (Map Int Int, Int, Int)
nextNum (m, i, 30000000) = EndF i
nextNum (m, i, n) =
  let nextI = maybe 0 (\last -> (n-1) - last) $ m Map.!? i
   in ContinueF (Map.insert i (n-1) m, nextI, n+1)

toMap :: [Int] -> (Map Int Int, Maybe Int, Int)
toMap = cata \case
  Nil -> (mempty, Nothing, -1)
  Cons a (m, Nothing, i) -> (m, Just a, i+1)
  Cons b (m, Just a, i) -> (Map.insert a i m, Just b, i+1)

partA :: Input -> OutputA
partA = hylo extractBeam nextNum . (\(a, Just b, c) -> (a, b, c+1)) . toMap . reverse

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
