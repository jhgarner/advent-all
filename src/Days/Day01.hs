module Days.Day01 (runDay) where

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
import Data.Functor.Foldable
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' decimal endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = fromJust . para (findIt 2020)

-- I think there's something interesting here about the structure but I'm not sure...
findIt :: Int -> ListF Int ([Int], Maybe Int) -> Maybe Int
findIt i (Cons x (xs, result)) = result <|> fmap (* x) (find (\y -> y + x == i) xs)
findIt _ Nil = Nothing

------------ PART B ------------
partB :: Input -> OutputB
partB = fromJust . para findItB
  
findItB :: ListF Int ([Int], Maybe Int) -> Maybe Int
findItB (Cons x (xs, result)) = result <|> fmap (* x) (para (findIt (2020 - x)) xs)
findItB Nil = Nothing
