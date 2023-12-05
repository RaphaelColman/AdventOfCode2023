module Solutions.Day5
    ( aoc5
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative.Combinators (some)
import           Text.Parser.Char                (letter, newline)
import           Text.Parser.Token               (TokenParsing (token))
import           Text.Trifecta                   (CharParsing (string), Parser,
                                                  count, integer, whiteSpace)

aoc5 :: IO ()
aoc5 = do
  printTestSolutions 5 $ MkAoCSolution parseInput part1
  --printSolutions 5 $ MkAoCSolution parseInput part2

data AlmanacComponent = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving
    ( Enum
    , Eq
    , Show
    )

type Range = (Integer, Integer, Integer)

data AlmanacMap
  = MkAlmanacMap
      { _from   :: !AlmanacComponent
      , _to     :: !AlmanacComponent
      , _ranges :: ![Range]
      }
  deriving (Eq, Show)

type Almanac = ([Integer], [AlmanacMap])

parseInput :: Parser Almanac
parseInput = do
  seeds <- parseSeeds
  maps <- some parseMap
  pure (seeds, maps)

parseMap :: Parser AlmanacMap
parseMap = do
  from <- some letter <* string "-to-" >>= toComponent
  to <- some letter <* string " map:" >>= toComponent
  newline
  ranges <- some $ token parseRange
  pure $ MkAlmanacMap from to ranges
  where toComponent str = case str of
          "seed"        -> pure Seed
          "soil"        -> pure Soil
          "fertilizer"  -> pure Fertilizer
          "water"       -> pure Water
          "light"       -> pure Light
          "temperature" -> pure Temperature
          "humidity"    -> pure Humidity
          "location"    -> pure Location
          _             -> fail "Unknown component"

parseRange :: Parser Range
parseRange = do
  [a,b,c] <- count 3 integer
  pure (a,b,c)

parseSeeds :: Parser [Integer]
parseSeeds = do
  string "seeds: "
  some integer

part1 = id

part2 :: String -> String
part2 = undefined
