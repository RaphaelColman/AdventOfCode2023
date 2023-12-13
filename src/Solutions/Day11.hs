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
import           Control.Monad.RWS               (modify)
import           Control.Monad.State             (MonadState (get), runState)
import           Data.Function                   ((&))
import           Data.Functor.Const
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import           Linear                          (V2 (V2))
import           Linear.V2                       (R1, _x, _y)
import           Linear.Vector                   (unit, (^*))
import           Text.Trifecta                   (CharParsing (anyChar), Parser)

type Galaxies = S.Set Point
data GalaxyState
  = MkState
      { _galaxies     :: !Galaxies
      , _emptyColumns :: ![Int]
      , _emptyRows    :: ![Int]
      }
  deriving (Eq, Show)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 :: M.Map Point Char -> Int
part1 = solve 1

part2 :: M.Map Point Char -> Int
part2 = solve (1000000-1)

solve :: Int -> M.Map Point Char -> Int
solve amount grid = sum $ map (uncurry manhattanDistance) pairs
  where expanded = expand amount $ toGalaxySet grid
        pairs = map (\[a, b] -> (a,b)) $ tuples 2 $ S.toList expanded


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

expandColumn :: Int -> Int -> Galaxies -> Galaxies
expandColumn amount col = S.map (\v -> if (v ^. _x) > col then v + (unit _x ^* amount) else v)

expandRow :: Int -> Int -> Galaxies -> Galaxies
expandRow amount row = S.map (\v -> if (v ^. _y) > row then v + (unit _y ^* amount) else v)

expand :: Int -> Galaxies -> Galaxies
expand amount galaxies = _galaxies finalState
  where emptyColumns' = emptyColumns galaxies
        emptyRows' = emptyRows galaxies
        initialState = MkState galaxies emptyColumns' emptyRows'
        finalState = snd . flip runState initialState $ do
            modify $ expandColumns amount
            modify $ expandRows amount

shortestDistance :: Point -> Point -> Int
shortestDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

expandColumns :: Int -> GalaxyState -> GalaxyState
expandColumns amount = go
  where go (MkState galaxies [] rows) = MkState galaxies [] rows
        go (MkState galaxies (col:cols) rows) = go $ MkState (expandColumn amount col galaxies) (map (+amount) cols) rows

expandRows :: Int -> GalaxyState -> GalaxyState
expandRows amount = go
  where go (MkState galaxies cols []) = MkState galaxies cols []
        go (MkState galaxies cols (row:rows)) = go $ MkState (expandRow amount row galaxies) cols (map (+amount) rows)
