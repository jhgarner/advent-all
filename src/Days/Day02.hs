module Days.Day02 (runDay) where

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
import Data.Attoparsec.Text hiding (count)
import Data.Text hiding (filter, length)
import qualified Data.Text as T (length)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' line endOfLine
  where line = do
          lb <- decimal
          char '-'
          ub <- decimal
          space
          c <- anyChar
          string ": "
          password <- takeTill (== '\n')
          return (lb, ub, c, password)

------------ TYPES ------------
type Input = [(Int, Int, Char, Text)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter (\(lb, ub, c, password) ->
                           let n = count (singleton c) password
                           in  n <= ub && n >= lb)

------------ PART B ------------
safeIndex :: Text -> Int -> Maybe Char
safeIndex t i
  | T.length t >= i = Just $ t `index` (i-1)
  | otherwise = Nothing
  
partB :: Input -> OutputB
partB = length . filter (\(a, b, c, password) ->
                           let loca = password `safeIndex` a
                               locb = password `safeIndex` b
                           in  (loca == Just c || locb == Just c) && loca /= locb)
