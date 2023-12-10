module Solutions.Day9
    ( aoc9
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative.Combinators (some)
import           Text.Parser.Char                (char)
import           Text.Parser.Token               (integer', token)
import           Text.Trifecta                   (Parser, integer, newline,
                                                  sepBy)
import Data.Foldable (Foldable(foldl'))

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

parseInput :: Parser [[Integer]]
parseInput = do
  xs <- sepBy (sepBy integer' (char ' ')) newline
  pure $ filter (not . null) xs --Because we're doing the newline separation ourselves it picks up an extra empty list at the end

part1 :: [[Integer]] -> Integer
part1 = solve nextValue

part2 :: [[Integer]] -> Integer
part2 = solve prevValue

solve :: Num c => (a -> c) -> [a] -> c
solve f = sum . map f

getDifferences :: [Integer] -> [Integer]
getDifferences xs = zipWith (-) (tail xs) xs

allDiffs :: [Integer] -> [[Integer]]
allDiffs xs = takeWhile (any (/=0)) $ iterate getDifferences xs

extrapolateForward :: [[Integer]] -> Integer
extrapolateForward = foldr accum 0
  where
    accum :: [Integer] -> Integer -> Integer
    accum xs acc = last xs + acc

nextValue :: [Integer] -> Integer
nextValue = extrapolateForward . allDiffs

extrapolateBackwards :: [[Integer]] -> Integer
extrapolateBackwards = foldr accum 0
  where
    accum :: [Integer] -> Integer -> Integer
    accum xs acc = head xs - acc

prevValue :: [Integer] -> Integer
prevValue = extrapolateBackwards . allDiffs
