{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
module Solutions.Day7
    ( aoc7
    ) where

import qualified Combinatorics.Coin        as M
import           Common.AoCSolutions       (AoCSolution (MkAoCSolution),
                                            printSolutions, printTestSolutions)
import           Common.ListUtils          (freqs)
import           Common.MapUtils           (maximumValue)
import           Control.Lens              (makeLenses)
import           Control.Lens.Combinators  (mapped)
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       (over, (.~))
import           Control.Monad.Combinators (some)
import           Data.List                 (sort)
import qualified Data.Map                  as M
import           Debug.Trace
import           Text.Parser.Char          (char)
import           Text.Parser.Token         (token)
import           Text.Trifecta             (Parser, alphaNum, brackets,
                                            commaSep, integer, semiSep,
                                            whiteSpace)
data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving
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

makeLenses ''Player

aoc7 :: IO ()
aoc7 = do
  --printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

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
part1 = solve

part2 :: [Player] -> Integer
part2 = solve . jacksToJokers

solve :: [Player] -> Integer
solve = sum . zipWith (*) [1..] . map _bid . sort

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
  xs              -> error $ "Invalid hand: " ++ show cards --Make this return a Maybe instead?
  where groupings = reverse $ sort $ map snd $ M.toList converted
        frqs = freqs cards
        numJokers = M.findWithDefault 0 Joker frqs
        (maxCard, frq) = maximumValue $ M.delete Joker frqs
        converted
          | numJokers == 0 = frqs
          | numJokers == 5 = frqs
          | otherwise = M.insertWith (+) maxCard numJokers frqs & M.delete Joker

jacksToJokers :: [Player] -> [Player]
jacksToJokers = over (mapped . hand) convertHand
  where convertHand hand = map (\card -> if card == Jack then Joker else card) hand
