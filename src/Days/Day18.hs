{-# LANGUAGE TemplateHaskell #-}

module Days.Day18 (runDay) where

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
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
{- ORMOLU_ENABLE -}

data Arith = Paren Arith
           | Mul Arith Arith
           | Add Arith Arith
           | Val Int
 deriving (Show, Eq)
makeBaseFunctor ''Arith


runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1' oneLine endOfLine
  where
    oneLine = do
      lhs <- paren <|> var
      op lhs
    paren = do
      string "("
      inside <- oneLine
      string ")"
      pure $ Paren inside
    mul lhs = do
      string " * "
      Mul lhs <$> ((var <|> paren) >>= opOnlyPlus)
    add lhs = do
      string " + "
      Add lhs <$> (var <|> paren)
    var = do
      Val <$> decimal
    opOnlyPlus lhs = do
      (add lhs >>= opOnlyPlus) <|> pure lhs
    op lhs = do
      newLhs <- mul lhs <|> add lhs
      op newLhs <|> pure newLhs



------------ TYPES ------------
type Input = [Arith]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
evalA :: ArithF Int -> Int
evalA (ValF i) = i
evalA (AddF a b) = a + b
evalA (MulF a b) = a * b
evalA (ParenF a) = a

partA :: Input -> OutputA
partA = sum . fmap (cata evalA)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
