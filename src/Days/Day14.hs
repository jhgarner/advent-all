{-# LANGUAGE OverloadedLists #-}

module Days.Day14 (runDay) where

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
import Control.Monad.State.Strict
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

type Memory = Map Int [Bool]
data Action = Mem Int [Bool] | Mask [Maybe Bool]
  deriving (Show, Eq)
------------ PARSER ------------
pad :: [Bool] -> [Bool]
pad ls = replicate (36 - length ls) False ++ ls

toBinary :: Int -> [Bool]
toBinary 0 = []
toBinary n
    | even n = False : toBinary (n `div` 2)
    | otherwise = True : toBinary (n `div` 2)
      
inputParser :: Parser Input
inputParser = flip sepBy1' endOfLine $ memLine <|> maskLine
  where
    toMask :: Char -> Maybe Bool
    toMask 'X' = Nothing
    toMask '1' = Just True
    toMask '0' = Just False
    memLine = do
      string "mem["
      loc <- decimal
      string "] = "
      val <- pad . reverse . toBinary <$> decimal
      pure $ Mem loc val
    maskLine = do
      string "mask = "
      l <- fmap toMask <$> many1' (letter <|> digit)
      pure $ Mask l


------------ TYPES ------------
type Input = [Action]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
applyMask :: [Bool] -> [Maybe Bool] -> [Bool]
applyMask inp mask = apply <$> zip inp mask
  where
    apply (a,Nothing) = a
    apply (_,Just b) = b

runMachine :: Action -> State (Memory, [Maybe Bool]) ()
runMachine (Mask m) = modify' (\(a, _) -> (a, m))
runMachine (Mem loc m) = do
  (mem, mask) <- get
  let masked = applyMask m mask
  let newMem = Map.insert loc masked mem
  put (newMem, mask)

toInt :: Int -> [Bool] -> Int
toInt 0 [] = 0
toInt n (b:bs) = 2^(n-1) * fromEnum b + toInt (n-1) bs

partA :: [Action] -> OutputA
partA inp = sum $ fmap (toInt 36) $ fst $ flip execState (mempty, replicate 36 Nothing) $ foldl' (\acc a -> acc >> runMachine a) (pure ()) inp

------------ PART B ------------

applyMaskB :: [Bool] -> [Maybe Bool] -> [Int]
applyMaskB inp mask = fmap (toInt 36) $ foldr apply [[]] $ zip inp mask
  where
    apply :: (Bool, Maybe Bool) -> [[Bool]] -> [[Bool]]
    apply (_,Nothing) acc = acc >>= \ls -> [True:ls,False:ls]
    apply (_,Just True) acc = acc >>= \ls -> [True:ls]
    apply (a,Just False) acc = acc >>= \ls -> [a:ls]

runMachineB :: Action -> State (Memory, [Maybe Bool]) ()
runMachineB (Mask m) = modify' (\(a, _) -> (a, m))
runMachineB (Mem loc m) = do
  (mem, mask) <- get
  let maskedLocs = applyMaskB (pad $ reverse $ toBinary loc) mask
  let newMem = foldl' (\acc l -> Map.insert l m acc) mem maskedLocs
  put (newMem, mask)
  
partB :: Input -> OutputB
partB inp = sum $ fmap (toInt 36) $ fst $ flip execState (mempty, replicate 36 Nothing) $ foldl' (\acc a -> acc >> runMachineB a) (pure ()) inp
