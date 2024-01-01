{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day3
    ( aoc3
    ) where

import           Combinatorics.Permutation.WithoutSomeFixpoints (numbers)
import           Common.AoCSolutions                            (AoCSolution (MkAoCSolution),
                                                                 printSolutions,
                                                                 printTestSolutions)
import           Common.FunctorUtils                            (fmap2)
import           Common.Geometry                                (Grid, Point,
                                                                 enumerateMultilineStringToVectorMap,
                                                                 gridNeighbours,
                                                                 neighbours,
                                                                 toOrderedList,
                                                                 toRows)
import           Control.Applicative.Combinators                (some)
import           Control.Lens                                   ()
import           Data.Char                                      (isDigit)
import           Data.Foldable                                  (maximumBy,
                                                                 minimumBy)
import           Data.List                                      (groupBy,
                                                                 sortBy)
import qualified Data.Map.Strict                                as M
import           Data.Monoid                                    (Sum (Sum),
                                                                 getSum)
import qualified Data.Set                                       as S
import           Debug.Trace                                    (traceShow)
import           Linear                                         (V2 (..))
import           Linear.V2                                      (_x, _y)
import           Safe                                           (headMay)
import           Text.Parser.Char                               (CharParsing (anyChar))
import           Text.Trifecta                                  (Parser)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 :: M.Map Point Char -> Int
part1 = solve


part2 :: Grid Char -> Int
part2 = gears


solve :: M.Map Point Char -> Int
solve grid = sum numbersNextToSymbols
  where symbols = M.filter (\c -> not (isDigit c || c == '.')) grid
        nums = fetchNumbers grid
        numbersNextToSymbols = convertToInts $ filter (isNextToSymbol grid) nums

convertToInts :: [[(Point, Char)]] -> [Int]
convertToInts = map read . fmap2 snd

fetchNumbers :: M.Map Point Char -> [[(Point, Char)]]
fetchNumbers grid = concatMap justNumbers asRows
  where asRows = toRows grid
        justNumbers row = filter (maybe False (isDigit . snd) . headMay) $
                          groupBy (\(_, c1) (_, c2) -> all isDigit [c1, c2] || (c1 == c2)) row

isNextToSymbol :: Grid Char -> [(Point, Char)] -> Bool
isNextToSymbol grid points = not (null adjacentSymbols)
    where neighbourBlock = S.unions $ map (neighbours . fst) points
          adjacentSymbols = M.filter (\c -> not (isDigit c || c == '.')) $ M.restrictKeys grid neighbourBlock

-- |Get map of every digit and the unique integer it is part of
-- The values are tuples of the form (Integer found in the map, Unique number)
digitLocations :: Grid Char -> M.Map Point (Int, Int)
digitLocations grid = M.fromList nums
  where nums = concatMap f $ zip [0..] $ fetchNumbers grid
        f :: (Int, [(Point, Char)]) -> [(Point, (Int, Int))]
        f (id, numAsList) = let theInteger :: Int  = read $ fmap snd numAsList
                            in map (\(p, _) -> (p, (theInteger, id))) numAsList

gears :: Grid Char -> Int
gears grid = getSum $ M.foldMapWithKey gearRatio asterisks
  where asterisks = M.filter (== '*') grid
        dl = digitLocations grid
        go = M.foldMapWithKey gearRatio asterisks
        gearRatio :: Point -> Char -> Sum Int
        gearRatio point _ = if length neighbouringInts == 2
                            then Sum $ product $ S.map fst neighbouringInts
                            else 0
          where neighbouringInts = S.fromList $ M.elems $ M.intersection dl $ gridNeighbours grid point
