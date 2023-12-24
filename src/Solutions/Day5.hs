{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Solutions.Day5
    ( aoc5
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.ListUtils                (window2)
import           Common.MaybeUtils               (firstJust)
import           Control.Applicative.Combinators (some)
import           Data.Foldable                   (Foldable (foldl'))
import qualified Data.Interval                   as IV
import qualified Data.IntervalMap.Strict         as IVM
import qualified Data.IntervalSet                as IVS
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Debug.Trace
import           Text.Parser.Char                (letter, newline)
import           Text.Parser.Token               (TokenParsing (token))
import           Text.Trifecta                   (CharParsing (string), Parser,
                                                  count, integer, whiteSpace)
import Data.List.Split (chunksOf)
import Data.IntegerInterval (lowerBound)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

data AlmanacComponent = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

type Range = (Integer, Integer, Integer)

data AlmanacMapRaw
  = MkAlmanacMapRaw
      { _fromComponent :: !AlmanacComponent
      , _toComponent   :: !AlmanacComponent
      , _ranges        :: ![Range]
      }
  deriving (Eq, Show)

data AlmanacMap
  = MkAlmanacMap
      { _from :: !AlmanacComponent
      , _to   :: !AlmanacComponent
      , _map  :: !(IVM.IntervalMap Integer Integer)
      }
  deriving (Eq, Show)

type Almanac = ([Integer], [AlmanacMapRaw])

parseInput :: Parser Almanac
parseInput = do
  seeds <- parseSeeds
  maps <- some parseMap
  pure (seeds, maps)

parseMap :: Parser AlmanacMapRaw
parseMap = do
  from <- some letter <* string "-to-" >>= toComponent
  to <- some letter <* string " map:" >>= toComponent
  newline
  ranges <- some $ token parseRange
  pure $ MkAlmanacMapRaw from to ranges
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


part1 :: ([Integer], [AlmanacMapRaw]) -> Integer
part1 (seeds, maps) = minimum $ map (`resolveSeed` organisedMaps) seeds
  where organisedMaps = organiseMaps maps

part2 :: ([Integer], [AlmanacMapRaw]) -> IVS.Extended Integer
part2 (seeds, maps) = minimum $ map IV.lowerBound $ IVS.toList locations
  where seedRanges = toSeedRanges seeds
        organisedMaps = organiseMaps maps
        locations = foldl' (\ranges component -> consultMapWithRanges (organisedMaps M.! component) ranges) seedRanges [Seed .. Humidity]

toAlamanacMap :: AlmanacMapRaw -> AlmanacMap
toAlamanacMap MkAlmanacMapRaw{..} = MkAlmanacMap _fromComponent _toComponent intervalMap
  where intervalMap = IVM.fromList $ map (\(dest, src, len) -> (fromRange src len, dest-src)) _ranges

fromRange :: Integer -> Integer -> IV.Interval Integer
fromRange x len = IV.Finite x IV.<=..<= IV.Finite (x + len)


consultMap :: AlmanacMap -> Integer -> Integer
consultMap MkAlmanacMap{..} value = value + IVM.findWithDefault 0 value _map

consultMapWithRanges :: AlmanacMap -> IVS.IntervalSet Integer -> IVS.IntervalSet Integer
consultMapWithRanges MkAlmanacMap{..} inputs = converted <> misses
  where tempMap = IVM.fromList $ map (, ()) $ IVS.toList inputs
        hits = IVM.intersectionWith const _map tempMap
        converted = IVS.fromList $ map (\(interval, modifier) -> IV.mapMonotonic (+ modifier) interval) $ IVM.toList hits
        misses = IVM.keysSet  $ tempMap IVM.\\ _map

organiseMaps :: [AlmanacMapRaw] -> M.Map AlmanacComponent AlmanacMap
organiseMaps maps = M.fromList $ map (\m -> (_fromComponent m, toAlamanacMap m)) maps

resolveSeed :: Integer -> M.Map AlmanacComponent AlmanacMap -> Integer
resolveSeed seed maps = foldl' foldOverMap seed [Seed .. Humidity]
  where getMap m = maps M.! m --Make this more type safe by handling nulls?
        foldOverMap value component = consultMap (getMap component) value

toSeedRanges :: [Integer] -> IVS.IntervalSet Integer
toSeedRanges = IVS.fromList . map (\[a, b] -> fromRange a b) . chunksOf 2
