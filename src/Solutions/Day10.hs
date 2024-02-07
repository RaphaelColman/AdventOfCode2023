{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solutions.Day10
    ( aoc10
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.Geometry                 (Grid, Point,
                                                  enumerateMultilineString,
                                                  enumerateMultilineStringToVectorMap,
                                                  gridNeighbours,
                                                  gridOrthogonalNeighbours)
import           Control.Applicative.Combinators (some)
import qualified Control.Monad                   as Data
import           Control.Monad.RWS               (MonadReader (ask),
                                                  MonadTrans (lift), asks)
import           Control.Monad.Reader            (Reader, ReaderT (runReaderT))
import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT))
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Data.Function                   ((&))
import           Data.Functor.Foldable           (ListF)
import           Data.Functor.Foldable.TH
import qualified Data.Map                        as M
import           Data.Maybe                      (mapMaybe)
import           Data.Sequence                   (Seq (Empty, (:<|), (:|>)),
                                                  ViewR ((:>)), (|>))
import qualified Data.Sequence                   as Seq (Seq, fromList, viewr, (!?), lookup, index)
import qualified Data.Set                        as S
import           Debug.Trace                     
import           Linear                          (V2 (V2), lookAt)
import           Linear.V2                       (V2)
import           Safe                            (headMay)
import           Text.Trifecta                   (CharParsing (anyChar), Parser)

data Cardinal = N | S | E | W deriving (Bounded, Enum, Eq, Ord, Show)

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  --printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 = solve

part2 :: String -> String
part2 = undefined

solve :: Grid Char -> Maybe Int
solve grid = do
  (start, _) <- findStart grid
  first <- pickFirstPipe grid start
  let path = traverseLoop grid start first
  pure $ length path `div` 2

findStart :: Grid Char -> Maybe (Point, Char)
findStart grid = let filtered = M.filter (=='S') grid
                 in M.toList filtered & headMay

-- | Given a point, look at the pipe and return a list of the neighbours to which it connects
getValidNeighbours :: Grid Char -> Point -> [Point]
getValidNeighbours grid p = M.keys $ M.restrictKeys grid (S.fromList lookups)
  where openC = openCardinals (grid M.! p)
        lookups = map (\c -> p + cardinalToVector c) openC

-- | This will just pick the first valid pipe next to the start.
pickFirstPipe :: Grid Char -> Point -> Maybe Point
pickFirstPipe grid start = headMay $ filter connectsToStart n
  where n = M.keys $ gridOrthogonalNeighbours grid start
        connectsToStart :: Point -> Bool
        connectsToStart point = start `elem` getValidNeighbours grid point

-- This could maybe use recursion-schemes ana if I could figure out how to make a baseFunctor
-- out of Data.Sequence
traverseLoop :: Grid Char -> Point -> Point -> Seq Point
traverseLoop grid start firstPipe = go grid $ Seq.fromList [start, firstPipe]
  where go grid' path@(rest :|> mostRecent :|> current)
                      | current == start = path
                      | otherwise = let validNeighbours = getValidNeighbours grid' current
                                        --Each pipe connects to two places, so there should only be one which is not the
                                        --one we just came from
                                        [next] = filter (/= mostRecent) validNeighbours
                                    in go grid' $ path |> next
        go grid' Empty = error "Empty sequence. This should not be possible"

--Just looked at the puzzle input and there are only two points which connect to the S. So I'm not really sure what the other
--pipes are for. Can I write this just asuming that the S only connects to two valid pipes?

-- Furthest point should be half way along the given path.
-- If it's an odd number of steps then great. Otherwise error for now
furthestPoint :: Seq Point -> Point
furthestPoint path = let halfWay = length path `div` 2
                     in Seq.index path halfWay

openCardinals :: Char -> [Cardinal]
openCardinals c = case c of
  '|' -> [N,S]
  '-' -> [E,W]
  'L' -> [N,E]
  'J' -> [N,W]
  '7' -> [S,W]
  'F' -> [S,E]
  '.' -> []
  'S' -> [N,S,E,W]
  _   -> error $ "Unknown character: " ++ [c]

cardinalToVector :: Cardinal -> V2 Int
cardinalToVector c = case c of
  N -> V2 0 (-1)
  S -> V2 0 1
  E -> V2 1 0
  W -> V2 (-1) 0
