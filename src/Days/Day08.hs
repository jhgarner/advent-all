{-# LANGUAGE TemplateHaskell #-}

module Days.Day08 (runDay) where

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
import Polysemy
import Polysemy.State

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import qualified Polysemy.NonDet as N
import Control.Applicative
{- ORMOLU_ENABLE -}

data OpMachine where
  Nop :: Int -> OpMachine
  Jmp :: Int -> OpMachine
  Acc :: Int -> OpMachine
  deriving (Show)

-- makeSem ''OpMachine

runOp :: Members '[State (Set Int), State Int] r => [OpMachine] -> Int -> Sem r Int
runOp program i = do
  visited <- get @(Set Int)
  put @(Set Int) $ i `Set.insert` visited
  if Set.member i visited
    then get @Int
    else
      case program !! i of
        Nop _ -> runOp program $ i + 1
        Jmp j -> runOp program $ i + j
        Acc j -> modify' (+ j) >> runOp program (i+1)
        
runOp2 :: Members '[State (Set Int), State Int, N.NonDet] r => [OpMachine] -> Int -> Int -> Sem r Int
runOp2 _ 2 _ = empty
runOp2 program changed i = do
  visited <- get @(Set Int)
  put @(Set Int) $ i `Set.insert` visited
  if
    | Set.member i visited -> empty
    | length program == i -> get @Int
    | i > length program -> empty
    | otherwise ->
      case program !! i of
        Nop j -> (runOp2 program changed $ i + 1) <|> (runOp2 program (changed+1) $ i + j)
        Jmp j -> (runOp2 program changed $ i + j) <|> (runOp2 program (changed+1) $ i + 1)
        Acc j -> modify' (+ j) >> runOp2 program changed (i+1)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ choice
  [ Nop <$> (string "nop " *> signed decimal)
  , Jmp <$> (string "jmp " *> signed decimal)
  , Acc <$> (string "acc " *> signed decimal)
  ]

------------ TYPES ------------
type Input = [OpMachine]

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
partA :: Input -> OutputA
partA m = run $ evalState @(Set Int) mempty $ evalState @Int 0 $ runOp m 0

------------ PART B ------------
partB :: Input -> OutputB
partB m = run $ N.runNonDetMaybe $ evalState @(Set Int) mempty $ evalState @Int 0 $ runOp2 m 0 0
