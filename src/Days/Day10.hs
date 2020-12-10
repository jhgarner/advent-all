module Days.Day10 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Set.fromList <$> sepBy1' decimal endOfLine

------------ TYPES ------------
type Input = Set Int

type OutputA = Int

type OutputB = Integer

------------ PART A ------------

partA :: Input -> OutputA
partA ads = uncurry (*) $ nextAdaptor 0
  where
    nextAdaptor :: Int -> (Int, Int)
    nextAdaptor n =
      if
        | Set.member (n+1) ads -> let (os, ts) = nextAdaptor (n+1) in (os+1,ts)
        | Set.member (n+2) ads -> nextAdaptor (n+2) 
        | Set.member (n+3) ads -> let (os, ts) = nextAdaptor (n+1) in (os,ts+1)
        | otherwise -> (0, 1)

------------ PART B ------------
partB :: Input -> OutputB
partB ads = allAdaptors 0
  where
    m = maximum ads
    -- Thanks HaskellWiki for giving me this code although I don't know why it memoizes...
    allAdaptors = (map allAds [0 ..] !!)
      where
        allAds n
          | n == m = 1
          | otherwise = sum $ fmap (\i -> if Set.member (n+i) ads then allAdaptors (n+i) else 0) [1, 2, 3]
    -- allAdaptors :: Int -> Integer
    -- allAdaptors n
    --   | n == m = 1
    --   | otherwise = sum $ fmap (\i -> if Set.member (n+i) ads then allAdaptors (n+i) else 0) [1, 2, 3]
      
