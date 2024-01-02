{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Solutions.Day4
    ( aoc4
    ) where

import           Combinatorics.Mastermind (Eval (white))
import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Common.MaybeUtils        (loopMaybe)
import           Common.SequenceUtils     (subSequence)
import           Control.Applicative      (Alternative (some))
import           Control.Lens             (makeLenses)
import           Control.Lens.Getter      ((^.))
import           Control.Monad.RWS.Strict (gets, modify)
import           Control.Monad.State      (execState, get)
import           Data.Foldable            (Foldable (toList), traverse_)
import qualified Data.IntMap              as IM
import           Data.List                (intersect, nub, union)
import qualified Data.Sequence            as S
import           Debug.Trace              (traceShow, traceShowM)
import           Text.Parser.Char         (CharParsing (char))
import           Text.Parser.Token        (integer, token)
import           Text.Trifecta            (CharParsing (string), Parser, sepBy,
                                           whiteSpace)

data Card
  = MkCard
      { _number  :: !Integer
      , _winners :: ![Integer]
      , _entries :: ![Integer]
      }
  deriving (Eq, Show)

data CardState
  = MkCardState
      { _unprocessedCards :: !(IM.IntMap Integer) -- The total count of each instance of that card of that index
      , _processedCards   :: !Integer
      }
  deriving (Eq, Show)

makeLenses ''Card
makeLenses ''CardState

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser [Card]
parseInput = some $ token parseCard

parseCard :: Parser Card
parseCard = do
  index <- string "Card" >> whiteSpace >> integer
  char ':' >> whiteSpace
  winners <- sepBy integer whiteSpace
  string "|" >> whiteSpace
  entries <- sepBy integer whiteSpace
  pure $ MkCard index winners entries

part1 :: [Card] -> Int
part1 cards = sum $ map cardPoints cards

part2 :: [Card] -> Maybe CardState
part2 cards = solve cardIndex initialState
  where cardIndex = S.fromList cards
        initialState = MkCardState unprocessed 0
        unprocessed = IM.fromList $ map (\c -> (fromInteger (c ^. number), 1)) cards

cardPoints :: Card -> Int
cardPoints MkCard {..} = convertToPoints $ length $ _winners `intersect` _entries
  where convertToPoints 0 = 0
        convertToPoints x = 2^(x-1)

processCard :: S.Seq Card -> CardState -> Maybe CardState
processCard cardIndex state@MkCardState{..} = do
          (currentCardIndex, number) <- IM.lookupMin _unprocessedCards
          currentCard <- S.lookup (currentCardIndex - 1) cardIndex
          let numWinners = length $ (currentCard ^. winners) `intersect` (currentCard ^. entries)
          let newUnprocessed = flip execState _unprocessedCards $ do
                                traverse_ (modify . IM.adjust (+number)) [currentCardIndex + 1.. currentCardIndex + numWinners]
                                modify $ IM.delete currentCardIndex
          pure $ MkCardState newUnprocessed (_processedCards + number)

solve :: S.Seq Card -> CardState -> Maybe CardState
solve cardIndex state = do
        loopMaybe (processCard cardIndex) (\s -> null (s ^. unprocessedCards)) state
