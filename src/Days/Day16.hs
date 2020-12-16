module Days.Day16 (runDay) where

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
import Data.Text (unpack, pack, Text)
import Control.Monad
import Data.Foldable
import Debug.Trace
{- ORMOLU_ENABLE -}

data Field = Field Text [(Int, Int)]
  deriving (Show, Eq)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,,) <$> many1' fields <*> yourTicket <*> nearbyTickets
  where
    fields = do
      name <- takeTill (== ':')
      string ": "
      ranges <- sepBy1' range $ string " or "
      endOfLine
      pure $ Field name ranges
    range = (,) <$> decimal <*> (char '-' *> decimal)
    yourTicket = do
      endOfLine
      string "your ticket:\n"
      nums <- sepBy1' decimal $ char ','
      endOfLine
      pure nums
    nearbyTickets = do
      endOfLine
      string "nearby tickets:\n"
      sepBy1' (sepBy1' decimal $ char ',') endOfLine
      

------------ TYPES ------------
type Input = ([Field], [Int], [[Int]])
-- type Input = ([Field], [Int], [[Int]])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
filterRanges :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
filterRanges i = filter (any (\(lb, ub) -> i >= lb && i <= ub))

partA :: Input -> OutputA
partA (fields, mine, tickets) = sum processed
  where
    -- allValid = replicate (length mine) $ fmap (\(Field _ rs) -> rs) fields
    allValid = fmap (\(Field _ rs) -> rs) fields
    processed = tickets >>= filter (\num -> null $ filterRanges num allValid)
    -- processed = foldl' (\stillValid nums ->
    --                       uncurry filterRanges <$> zip nums stillValid) allValid tickets

------------ PART B ------------
findRequired :: [[[(Int, Int)]]] -> [[(Int, Int)]]
findRequired allItems =
  if all (\f -> length f == 1) allItems
  then join allItems
  else findRequired onlyAmbigous
  where
    unambiguous = join $ filter (\fields -> length fields == 1) allItems
    onlyAmbigous = map (\fields -> if length fields == 1
                                      then fields
                                      else filter (not . (`elem` unambiguous)) fields
                       ) allItems

partB :: Input -> OutputB
partB (fields, mine, tickets) = product onlyDepartures
  where
    allValid = replicate (length mine) $ fmap (\(Field _ rs) -> rs) fields
    allValidSmall = fmap (\(Field _ rs) -> rs) fields
    noInvalid = filter (not . any (\num -> null $ filterRanges num allValidSmall)) tickets
    processed = foldl' (\stillValid nums ->
                          uncurry filterRanges <$> zip nums stillValid) allValid noInvalid
    onlyDepartures :: [Int]
    onlyDepartures =
      mapMaybe (\(rs, i) -> asum $ fmap (\(Field name rs') ->
                                            if rs' == rs && isPrefixOf "departure" (unpack name)
                                            then Just i
                                            else Nothing) fields) $ zip (findRequired processed) mine
