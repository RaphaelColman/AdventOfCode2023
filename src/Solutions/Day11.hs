{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day11
    ( aoc11
    ) where

import           Combinatorics
import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.Debugging                (traceLns)
import           Common.Geometry                 (Grid, Point,
                                                  enumerateMultilineStringToVectorMap,
                                                  manhattanDistance,
                                                  renderVectorSet)
import           Control.Applicative.Combinators (some)
import           Control.Lens                    (Lens')
import           Control.Lens.Getter             ((^.))
import           Data.Function                   ((&))
import           Data.Functor.Const
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import           Linear                          (V2 (V2))
import           Linear.V2                       (R1, _x, _y)
import           Linear.Vector                   (unit)
import           Text.Trifecta                   (CharParsing (anyChar), Parser)

type Galaxies = S.Set Point

aoc11 :: IO ()
aoc11 = do
  --printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 :: M.Map Point Char -> Int
part1 grid = solve grid

solve :: M.Map Point Char -> Int
solve grid = sum $ map (uncurry manhattanDistance) pairs
  where expanded = expand $ toGalaxySet grid
        pairs = map (\[a, b] -> (a,b)) $ tuples 2 $ S.toList expanded


part2 = id

toGalaxySet :: Grid Char -> S.Set Point
toGalaxySet grid = M.keysSet $ M.filter (=='#') grid

emptyColumns :: Galaxies -> [Int]
emptyColumns galaxies = empty galaxies _x

emptyRows :: Galaxies -> [Int]
emptyRows galaxies = empty galaxies _y

empty :: Galaxies -> ((Int -> Const Int Int) -> Point -> Const Int Point) -> [Int]
empty galaxies f = filter ((==0) . numGalaxiesInColumn) [0..(max-1)]
  where max = maximum $ S.map (^. f) galaxies
        numGalaxiesInColumn i = length $ S.filter ((==i) . (^. f)) galaxies

expandColumn :: Int -> Galaxies -> Galaxies
expandColumn col = S.map (\v -> if (v ^. _x) > col then v + unit _x else v)

expandRow :: Int -> Galaxies -> Galaxies
expandRow row = S.map (\v -> if (v ^. _y) > row then v + unit _y else v)

expand :: Galaxies -> Galaxies
expand galaxies = foldr expandColumn galaxies emptyColumns'
                  & flip (foldr expandRow) emptyRows'
  where emptyColumns' = emptyColumns galaxies
        emptyRows' = emptyRows galaxies

shortestDistance :: Point -> Point -> Int
shortestDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)
