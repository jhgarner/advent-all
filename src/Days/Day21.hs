module Days.Day21 (runDay) where

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
import Data.Foldable
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ do
  names <- sepBy1' (many1' letter) space
  string " (contains "
  alergens <- sepBy1' (many1' letter) $ string ", "
  char ')'
  pure (names, alergens)

------------ TYPES ------------
type Input = [([String], [String])]

type OutputA = Int

type OutputB = String

------------ PART A ------------
addFood :: Map String (Set String) -> ([String], [String]) -> Map String (Set String)
addFood m (ingreds, alergens) =
  foldl' (\newM alergen ->
            Map.alter (\case
              Nothing -> Just $ Set.fromList ingreds
              Just s -> Just $ Set.intersection s $ Set.fromList ingreds)
            alergen newM
         ) m alergens

allFoods :: Input -> Map String (Set String)
allFoods = foldl' addFood mempty

allIngredients :: Input -> Set String
allIngredients = foldMap $ Set.fromList . fst

diffIngred :: Set String -> Map String (Set String) -> Set String
diffIngred s m = s Set.\\ fold m

countIngred :: Input -> Set String -> Int
countIngred ls s = sum $ sum . fmap (fromEnum . (`Set.member` s)) . fst <$> ls

partA :: Input -> OutputA
partA ls = countIngred ls $ diffIngred (allIngredients ls) $ allFoods ls

------------ PART B ------------
dangerousFor ::  Map String (Set String) -> Map String String
dangerousFor m =
  case Map.minViewWithKey $ Map.filter ((== 1) . Set.size) m of
    Nothing ->
      mempty
    Just ((k, v), _) ->
      let ingredient = head $ Set.toList v
       in Map.insert k ingredient $ dangerousFor $ Set.delete ingredient <$> Map.delete k m

showIt :: Map String String -> String
showIt = tail . foldl' (\acc s -> acc ++ "," ++ s) ""

partB :: Input -> OutputB
partB = showIt . dangerousFor . allFoods
