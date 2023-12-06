module Solutions.Day6
    ( aoc6
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative.Combinators (some)
import           Text.Trifecta                   (CharParsing (string), Parser,
                                                  TokenParsing (token), integer,
                                                  newline, whiteSpace)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

type Race = (Integer, Integer)

parseInput :: Parser [Race]
parseInput = do
  string "Time:" <* whiteSpace
  raceLengths <- some $ token integer
  string "Distance:" <* whiteSpace
  distances <- some $ token integer
  pure $ zip raceLengths distances

part1 :: [Race] -> Integer
part1 = product . map evaluateRace

part2 :: [Race] -> Integer
part2 = evaluateRace . combineRaces

combineRaces :: [Race] -> Race
combineRaces races = (combine lengths, combine distances)
  where lengths = map fst races
        distances = map snd races
        combine xs = read $ concatMap show xs

solveQuadratic :: Floating a => a -> a -> a -> (a, a)
solveQuadratic a b c = (plus, minus)
  where plus = (-b + sqrt (b^2 - 4*a*c)) / (2*a)
        minus = (-b - sqrt (b^2 - 4*a*c)) / (2*a)

evaluateRace :: Integral a => (Integer, Integer) -> a
evaluateRace (raceLength, distance) = floor minus - ceiling plus + 1
  where (plus, minus) = solveQuadratic (-1) (fromInteger raceLength) (fromInteger (-distance))
