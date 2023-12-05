{-# LANGUAGE RecordWildCards #-}
module Solutions.Day4
    ( aoc4
    ) where

import           Combinatorics.Mastermind (Eval (white))
import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Control.Applicative      (Alternative (some))
import           Data.List                (intersect, union)
import           GHC.OldList              (nub)
import           Text.Parser.Char         (CharParsing (char))
import           Text.Parser.Token        (integer, token)
import           Text.Trifecta            (CharParsing (string), Parser, sepBy,
                                           whiteSpace)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  --printSolutions 4 $ MkAoCSolution parseInput part2

data Card
  = MkCard
      { _winners :: ![Integer]
      , _entries :: ![Integer]
      }
  deriving (Eq, Show)

data CardState
  = MkCardState
      { _unprocessedCards       :: ![Card]
      , _total :: !Integer
      }
  deriving (Eq, Show)

parseInput = some $ token parseCard

parseCard :: Parser Card
parseCard = do
  string "Card" >> whiteSpace >> integer >> char ':' >> whiteSpace
  winners <- sepBy integer whiteSpace
  string "|" >> whiteSpace
  entries <- sepBy integer whiteSpace
  pure $ MkCard winners entries

part1 cards = sum $ map cardPoints cards

part2 :: String -> String
part2 = id

numWinners :: Card -> [Integer]
numWinners MkCard {..} = _entries `intersect` _winners

cardPoints :: Card -> Int
cardPoints MkCard {..} = convertToPoints $ length $ _winners `intersect` _entries
  where convertToPoints 0 = 0
        convertToPoints x = 2^(x-1)

processCard :: CardState -> CardState
processCard MkCardState{..} = undefined
