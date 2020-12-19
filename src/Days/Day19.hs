{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day19 (runDay) where

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
import Control.Applicative
import Debug.Trace
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Foldable
{- ORMOLU_ENABLE -}

data Rule = Lit Char | Pipe [[Rule]]
  deriving (Show, Eq)

makeBaseFunctor ''Rule
  
instance Show (RuleF Int) where
  show = const "No"

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (rules <* endOfLine) <*> input
  where
    rules = fmap Map.fromList $ flip sepBy1' endOfLine $ do
      ident <- decimal
      char ':'
      lhs <- lit <|> pipe
      pure $ (ident, lhs)
    lit = LitF <$> (string " \"" *> anyChar <* char '"')
    ref = char ' ' *> decimal
    chain = many1' ref
    pipe = PipeF <$> sepBy1' chain (string " |")
    input = endOfLine >> sepBy1' (many1' letter) endOfLine
    

------------ TYPES ------------
toFixed :: Map Int (RuleF Int) -> Int -> RuleF Int
toFixed m i = m Map.! i

parseInput :: RuleF (String -> Maybe String) -> String -> Maybe String
parseInput (LitF _) [] = Nothing
parseInput (LitF l) s = if head s == l then Just $ tail s else Nothing
parseInput (PipeF ls) s = asum $ fmap (foldl' (>>=) $ Just s) ls

parseInputB :: RuleF ([String] -> [Maybe String]) -> [String] -> [Maybe String]
parseInputB _ [] = []
parseInputB (LitF l) ls = filter (not . null) $ fmap (\case
                                                        c:cs -> if c == l then Just cs else Nothing
                                                        [] -> Nothing) ls
parseInputB (PipeF ls) ss = filter (not . null) $ concatMap (($ ss) . foldl1 (\acc a -> a . fmap fromJust . acc)) ls

type Input = (Map Int (RuleF Int), [String])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (m, inputs) = length $ filter (== Just "") $ fmap (cata parseInput rule) inputs
  where
    rule :: Rule
    rule = ana (toFixed m) 0

------------ PART B ------------
partB :: Input -> OutputB
partB (m, inputs) = length $ filter (elem $ Just "") $ fmap (cata parseInputB rule . pure) inputs
  where
    rule :: Rule
    rule = ana (toFixed m) 0
