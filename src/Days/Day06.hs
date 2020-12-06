{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import GHC.Exts (fromList)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Util.Util
import Data.Monoid
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Coerce (Coercible)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  flip sepBy1' (endOfLine *> endOfLine) $
    fromList <$> sepBy1' (many1' letter) endOfLine

------------ TYPES ------------
type Input = [NonEmpty [Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
sumCombinedSets
  :: (Coercible (Set a) b, Semigroup b, Ord a)
  => (Set a -> b)
  -> [NonEmpty [a]]
  -> Int
sumCombinedSets f = foldMapN Sum (length . foldMap1N f fromList)

partA :: Input -> OutputA
partA = sumCombinedSets id

------------ PART B ------------
partB :: Input -> OutputB
partB = sumCombinedSets Intersection
