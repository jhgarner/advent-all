{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}

module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (insert)
import Data.Map.Strict (Map, (!?), fromList, keys)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set, insert)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Util.Util
import Data.Monoid

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Text (Text)
import Data.Foldable
{- ORMOLU_ENABLE -}

data Bag = Bag String [(Int, Bag)]

makeBaseFunctor ''Bag

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB


------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' (string ".\n") $ do
  key <- manyTill' anyChar (string " bags contain ")
  values <- flip sepBy1' (string ", ") $ do
    n <- (decimal <* space) <|> pure 0
    name <- manyTill' anyChar (string " bags" <|> string " bag")
    pure (n, name)
  pure (key, values)

------------ TYPES ------------
type Input = [(String, [(Int, String)])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
toBag :: Map String [(Int, String)] -> String -> Bag
toBag m name = Bag name $ fmap (fmap $ toBag m) $ fromMaybe [] $ m !? name

mapToBag :: Map String [(Int, String)] -> [Bag]
mapToBag m = fmap (toBag m) $ keys m

countBag :: String -> BagF Bool -> Bool
countBag lookingFor (BagF name ls)
  | name == lookingFor = True
  | otherwise = any snd ls

partA :: Input -> OutputA
partA = length . filter id . fmap (cata (countBag "shiny gold")) . mapToBag . fromList

------------ PART B ------------
  
costBag :: BagF Int -> Int
costBag (BagF _ ls) = 1 + foldMapN Sum (uncurry (*)) ls

partB :: Input -> OutputB
partB = cata costBag . flip toBag "shiny gold" . fromList
