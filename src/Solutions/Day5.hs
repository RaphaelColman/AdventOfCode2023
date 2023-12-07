{-# LANGUAGE RecordWildCards #-}
module Solutions.Day5
    ( aoc5
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.MaybeUtils               (firstJust)
import           Control.Applicative.Combinators (some)
import           Data.Foldable                   (Foldable (foldl'))
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Debug.Trace
import           Text.Parser.Char                (letter, newline)
import           Text.Parser.Token               (TokenParsing (token))
import           Text.Trifecta                   (CharParsing (string), Parser,
                                                  count, integer, whiteSpace)

aoc5 :: IO ()
aoc5 = do
  --printSolutions 5 $ MkAoCSolution parseInput part1
  printTestSolutions 5 $ MkAoCSolution parseInput part2

data AlmanacComponent = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
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


part1 :: ([Integer], [AlmanacMap]) -> Integer
part1 (seeds, maps) = minimum $ map (`resolveSeed` organisedMaps) seeds
  where organisedMaps = organiseMaps maps

part2 = id


consultMap :: AlmanacMap -> Integer -> Integer
consultMap MkAlmanacMap{..} value = fromMaybe value mapped
  where consultRange (dest, source, range) = if (value >= source) && (value <= source + range)
                                             then Just $ value - source + dest
                                             else Nothing
        mapped = firstJust $ map consultRange _ranges

organiseMaps :: [AlmanacMap] -> M.Map AlmanacComponent AlmanacMap
organiseMaps maps = M.fromList $ map (\m -> (_from m, m)) maps

resolveSeed :: Integer -> M.Map AlmanacComponent AlmanacMap -> Integer
resolveSeed seed maps = foldl' foldOverMap seed [Seed .. Humidity]
  where getMap m = maps M.! m --Make this more type safe by handling nulls?
        foldOverMap value component = consultMap (getMap component) value

seedRanges :: [Integer] -> [Integer]
seedRanges = undefined
