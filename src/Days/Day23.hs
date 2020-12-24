{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import qualified Util.Util as U
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), Seq)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Debug.Trace
import System.IO.Unsafe
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Seq.fromList <$> many1' (fmap (read . (: [])) digit)

------------ TYPES ------------
type Input = Seq Int

type OutputA = String

type OutputB = Seq Int

------------ PART A ------------
extract3 :: Input -> (Seq Int, Input)
extract3 (l:<|ls) = (:<|) l <$> Seq.splitAt 3 ls

destNumber :: Seq Int -> Int -> Int -> Int
destNumber taken maxVal target
  | target < 1 = destNumber taken maxVal maxVal
  | target `elem` taken = destNumber taken maxVal $ target - 1
  | otherwise = target

placeAfter :: Seq Int -> Int -> Input -> Input
placeAfter taken target ls =
  let (lh, rh) = Seq.splitAt ((+1) $ fromJust $ Seq.elemIndexL target ls) ls
   in lh <> taken <> rh

gameA :: Int -> Input -> Input
gameA 0 ls = ls
gameA i ls@(oldH:<|_) = 
  let (taken, withRemoved) = extract3 ls
      dest = destNumber taken (maximum ls) (oldH - 1)
      (h:<|replaced) = placeAfter taken dest withRemoved
      nextStep = replaced :|> h
   in gameA (i-1) nextStep

after1 :: Input -> String
after1 ls = foldl' (\acc a -> acc ++ show a) "" $ Seq.take (length ls - 1) . Seq.drop 1 . Seq.dropWhileL (/= 1) $ Seq.cycleTaking (2 * length ls) ls

partA :: Input -> OutputA
partA = after1 . gameA 100

------------ PART B ------------
after1B :: Input -> Input
after1B ls = Seq.take 2 . Seq.drop 1 . Seq.dropWhileL (/= 1) $ Seq.cycleTaking (2 * length ls) ls

interleave a b = concat $ zipWith ((. return) . (:)) a b

findM :: (a -> IO Bool) -> [a] -> IO a
findM c (l:ls) = c l >>= \b -> if b then pure l else findM c ls

placeAfterB :: Int -> MVec.IOVector (Int, Int) -> Seq Int -> Int -> Input -> IO (Input, Int, Bool)
placeAfterB currentAge m taken target ls = do
  (start, age) <- m `MVec.read` target
  -- print $ (length ls, currentAge, taken, target, age)
  foundAt <- findM (\a -> MVec.write m (fromMaybe (error $ show (length ls, currentAge, taken, target, age, Seq.elemIndexL target ls)) $ Seq.lookup a ls) (a, currentAge) >> pure (Seq.lookup a ls == Just target)) $
    interleave ([max (start + (currentAge - age) - 4) 0 .. 999996] <> repeat 999996) $
             -- [max (start + (currentAge - age) - 3) 0 .. 1000000]
    if (start + (currentAge - age) - 4) > 0 then [(start + (currentAge - age) - 4), (start + (currentAge - age) - 5) .. 0] <> repeat 0 else repeat 0
  -- let foundAt = fromMaybe (error $ show (currentAge, age, start, target, Seq.elemIndexL target ls, start + (currentAge - age) )) $ find (\a -> Seq.lookup a ls == Just target) [max (start + (currentAge - age) * 4) 0 .. 1000000] -- [(start + (currentAge - age)), (start + (currentAge - age) - 1) .. -10000000]
      -- foundAt = traceShowId $ fromMaybe (error $ show (currentAge, age, start, target, Seq.elemIndexL target ls)) $ find (\a -> Seq.lookup a ls == Just target) [start - (currentAge - age) * 4 .. 1000000]
      -- foundAt = fromMaybe ((+1) $ fromJust $ Seq.elemIndexL target ls) $ find (\a -> Seq.lookup a ls == Just target) [start + (currentAge - age) .. 1000000]
  let reSearch = abs (foundAt - start) >= 250000 && False
      inserted = foldr (Seq.insertAt (foundAt+1)) ls taken
  pure $ if reSearch || currentAge `mod` 10000 == 0
         then traceShow (currentAge, age, start, target, foundAt, start + (currentAge - age) * 4)
              (inserted, foundAt, reSearch) else (inserted, foundAt, reSearch)

gameB :: Int -> MVec.IOVector (Int, Int) -> Input -> IO Input
gameB 0 _ !ls = pure ls
gameB !i m ls@(h:<|_) = do
  let (taken, withRemoved) = extract3 ls
      dest = destNumber taken 1000000 (h - 1)
  (newH:<|replaced, loc, reSearch) <- placeAfterB i m taken dest withRemoved
  let !nextStep = replaced :|> newH
  MVec.write m dest (loc, i)
  MVec.write m newH (1000000, i)
  foldl' (\acc took -> acc >> MVec.write m took (loc, i)) (pure ()) taken
  if reSearch then fst $ foldl' (\(acc, j) a -> (acc >> MVec.write m a (j, i), j+1)) (pure (), 0) nextStep else pure ()
  gameB (i-1) m nextStep

initialLoc :: IO (MVec.IOVector (Int, Int))
initialLoc = Vec.thaw $ Vec.fromList $ replicate 10 (0, 10000000) ++ zip [10..1000000] (repeat 10000000)

partB :: Input -> OutputB
partB i = unsafePerformIO $ do
  v <- initialLoc
  result <- gameB 10000000 v (i <> Seq.fromList [10..1000000])
  pure $ after1B result
