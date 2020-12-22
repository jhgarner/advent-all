module Days.Day22 (runDay) where

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
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), Seq)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Player 1:\n"
  p1Deck <- sepBy1' decimal endOfLine
  endOfLine >> endOfLine
  string "Player 2:\n"
  p2Deck <- sepBy1' decimal endOfLine
  pure (Seq.fromList p1Deck, Seq.fromList p2Deck)
  

------------ TYPES ------------
type Input = (Seq Int, Seq Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
playRound :: Input -> Seq Int
playRound (Empty, winner) = winner
playRound (winner, Empty) = winner
playRound (p1 :<| d1, p2 :<| d2)
  | p1 > p2 = playRound (d1 :|> p1 :|> p2, d2)
  | p1 < p2 = playRound (d1, d2 :|> p2 :|> p1)
  | otherwise = error $ show p1 ++ " " ++ show p2 ++ " EQUAL CARDS!"

scoreRound :: Seq Int -> Int
scoreRound = snd . foldr (\i (mult, score) -> (mult+1, mult * i + score)) (1, 0)

partA :: Input -> OutputA
partA = scoreRound . playRound

------------ PART B ------------
-- Turns out the map thing wasn't necessary but I'll leave it in even if it really complicates the code
playRecRound :: Map Input (Seq Int, Int) -> Set Input -> Input -> (Seq Int, Int, Map Input (Seq Int, Int))
playRecRound m _ (Empty, winner) = (winner, 2, m)
playRecRound m _ (winner, Empty) = (winner, 1, m)
playRecRound m s state@(p1 :<| d1, p2 :<| d2)
  | Map.member state m = let (a, b) = m Map.! state in (a, b, m)
  | Set.member state s = (p1 :<| d1, 1, Map.insert state (p1 :<| d1, 1) m)
  | (length d1 < p1 || length d2 < p2) && p1 > p2 = playRecRound m (Set.insert state s) (d1 :|> p1 :|> p2, d2)
  | (length d1 < p1 || length d2 < p2) && p1 < p2 = playRecRound m (Set.insert state s) (d1, d2 :|> p2 :|> p1)
  | otherwise = case playRecRound m (Set.insert state s) (Seq.take p1 d1, Seq.take p2 d2) of
                  (w, 1, newM) -> playRecRound (Map.insert (Seq.take p1 d1, Seq.take p2 d2) (w, 1) newM) (Set.insert state s) (d1 :|> p1 :|> p2, d2)
                  (w, 2, newM) -> playRecRound (Map.insert (Seq.take p1 d1, Seq.take p2 d2) (w, 2) newM) (Set.insert state s) (d1, d2 :|> p2 :|> p1)
  
partB :: Input -> OutputB
partB = scoreRound . (\(a, _, _) -> a) . playRecRound mempty mempty
