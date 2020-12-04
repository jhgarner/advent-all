{-# LANGUAGE TypeApplications #-}

module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (take)
import Prelude hiding (take)
import Data.Map.Strict (Map, fromList, member, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Text (Text, unpack, pack)

import qualified Program.RunDay as R (runDay)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Void
import Control.Monad (when)
import Control.Monad.Fail (fail)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = map fromList <$> many1' ((many1' kv <* endOfLine) <|> emptyOne)
  where
    emptyOne :: Parser [(Text, Text)]
    emptyOne = do
      manyTill' anyChar (endOfLine *> endOfLine)
      pure []
    kv = do
      key <- takeTill (\c -> elem @[] c ":\n")
      char ':'
      val <- case key of
        "byr" -> do
          value <- decimal
          when (value < 1920 || value > 2002) $
            fail "byr"
          pure $ pack $ show value
        "iyr" -> do
          value <- decimal
          when (value < 2010 || value > 2020) $
            fail "iyr"
          pure $ pack $ show value
        "eyr" -> do
          value <- decimal
          when (value < 2020 || value > 2030) $
            fail "eyr"
          pure $ pack $ show value
        "hgt" -> do
          value <- decimal
          unit <- choice
            [ do
                string "cm"
                when (value < 150 || value > 193) $
                    fail "cm"
                pure "cm"
            , do
                string "in"
                when (value < 59 || value > 76) $
                    fail "inches"
                pure "in"
            ]
          pure $ pack $ show value ++ unit
        "hcl" -> do
          char '#'
          color <- take 6
          when (any (not . inClass "a-f0-9") $ unpack color) $
            fail "hcl"
          pure $ pack $ "#" ++ unpack color
        "ecl" -> do
          choice $ map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        "pid" -> do
          pid <- take 9
          when (any (not . inClass "0-9") $ unpack pid) $
            fail "pid"
          pure pid
        _ -> do
          takeTill (\c -> elem @[] c " \n")
      char ' ' <|> char '\n'
      pure (key, val)
  

------------ TYPES ------------
type Input = [Map Text Text]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
-- Part A is actually part B right now. I replaced the parser with a more complicated one
required :: [Text]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

partA :: Input -> OutputA
partA ms = length $ filter (\m -> all (`member` m) required) ms

------------ PART B ------------
-- validate :: Map Text Text -> Bool
-- validate m = and
  -- [ m ! "byr" >= 1920 && m ! "byr" < 
  -- ]

partB :: Input -> OutputB
partB = error "Not implemented yet!"
