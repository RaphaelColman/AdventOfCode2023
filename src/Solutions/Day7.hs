{-# LANGUAGE InstanceSigs #-}
module Solutions.Day7
    ( aoc7
    ) where

import qualified Combinatorics.Coin        as M
import           Common.AoCSolutions       (AoCSolution (MkAoCSolution),
                                            printSolutions, printTestSolutions)
import           Common.ListUtils          (freqs)
import           Control.Monad.Combinators (some)
import           Data.List                 (sort)
import qualified Data.Map                  as M
import           Text.Parser.Char          (char)
import           Text.Parser.Token         (token)
import           Text.Trifecta             (Parser, alphaNum, brackets,
                                            commaSep, integer, semiSep,
                                            whiteSpace)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  --printSolutions 7 $ MkAoCSolution parseInput part2

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

-- A hand and a bid
type Hand = [Card]

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

data Player
  = MkPlayer
      { _hand :: !Hand
      , _bid  :: !Integer
      }
  deriving (Eq, Show)

parseInput :: Parser [Player]
parseInput = some $ token parseMove

parseMove :: Parser Player
parseMove = do
  h <- parseHand <* whiteSpace
  MkPlayer h <$> integer

parseHand :: Parser [Card]
parseHand = some parseCard

parseCard :: Parser Card
parseCard = do
  c <- alphaNum
  case c of
    '2' -> pure Two
    '3' -> pure Three
    '4' -> pure Four
    '5' -> pure Five
    '6' -> pure Six
    '7' -> pure Seven
    '8' -> pure Eight
    '9' -> pure Nine
    'T' -> pure Ten
    'J' -> pure Jack
    'Q' -> pure Queen
    'K' -> pure King
    'A' -> pure Ace
    _   -> fail "Invalid card"

part1 :: [Player] -> Integer
part1 = sum . zipWith (*) [1..] . map _bid . sort

part2 :: String -> String
part2 = undefined

instance Ord Player where
  compare :: Player -> Player -> Ordering
  compare (MkPlayer h1 b1) (MkPlayer h2 b2) = let compareHandTypes = compare handType1 handType2 in
                                                if compareHandTypes == EQ
                                                then compare h1 h2
                                                else compareHandTypes
                                      where handType1 = handType h1
                                            handType2 = handType h2

handType :: Hand -> HandType
handType cards = case groupings of
  [1, 1, 1, 1, 1] -> HighCard
  [2, 1, 1, 1]    -> OnePair
  [2, 2, 1]       -> TwoPair
  [3, 1, 1]       -> ThreeOfAKind
  [3, 2]          -> FullHouse
  [4, 1]          -> FourOfAKind
  [5]             -> FiveOfAKind
  xs              -> error $ "Invalid hand: " ++ show xs --Make this return a Maybe instead?
  where groupings = reverse $ sort $ map snd $ M.toList $ freqs cards
