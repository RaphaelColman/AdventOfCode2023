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
                                                  gridOrthogonalNeighbours,
                                                  toColumns, toRows)
import           Control.Applicative.Combinators (some)
import           Control.Monad                   (filterM)
import           Control.Monad.RWS.Strict        (MonadReader (ask),
                                                  MonadTrans (lift), asks)
import           Control.Monad.Reader            (Reader, ReaderT (runReaderT),
                                                  runReader)
import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Data.Foldable                   (Foldable (toList), foldlM)
import           Data.Function                   ((&))
import           Data.Functor.Foldable           (ListF)
import           Data.Functor.Foldable.TH
import           Data.List                       (intersect, intersectBy)
import qualified Data.Map                        as M
import           Data.Maybe                      (mapMaybe)
import           Data.Sequence                   (Seq (Empty, (:<|), (:|>)),
                                                  ViewR ((:>)), (|>))
import qualified Data.Sequence                   as Seq (Seq, fromList, index,
                                                         lookup, viewr, (!?))
import qualified Data.Set                        as S
import           Debug.Trace
import           Linear                          (V2 (V2), lookAt)
import           Linear.V2                       (V2)
import           Safe                            (headMay)
import           Text.Trifecta                   (CharParsing (anyChar), Parser)

data Cardinal = N | S | E | W deriving (Bounded, Enum, Eq, Ord, Show)

aoc10 :: IO ()
aoc10 = do
  --printTestSolutions 10 $ MkAoCSolution parseInput part1
  printTestSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 :: Grid Char -> Maybe Int
part1 grid = do
  path <- runReader (runMaybeT solvePath) grid
  pure $ length path `div` 2

part2 :: Grid Char -> Maybe [Point]
part2 grid = do
  runReader (runMaybeT solvePart2) grid
  where cols = toColumns grid

solvePart2 :: MaybeT (Reader (Grid Char)) [Point]
solvePart2 = do
  path <- solvePath
  lift $ insidePoints path

solvePath :: MaybeT (Reader (Grid Char)) (Seq Point)
solvePath = do
  (start, _) <- findStart
  first <- pickFirstPipe start
  lift $ traverseLoop start first

findStart :: MaybeT (Reader (Grid Char)) (Point, Char)
findStart = do
  grid <- ask
  let filtered = M.filter (=='S') grid
  M.toList filtered & headMay & MaybeT . pure

-- | Given a point, look at the pipe and return a list of the neighbours to which it connects
getValidNeighbours :: Point -> Reader (Grid Char) [Point]
getValidNeighbours p = do
  grid <- ask
  let openC = openCardinals (grid M.! p)
  let lookups = map (\c -> p + cardinalToVector c) openC
  pure $ M.keys $ M.restrictKeys grid (S.fromList lookups)

-- | This will just pick the first valid pipe next to the start.
pickFirstPipe :: Point -> MaybeT (Reader (Grid Char)) Point
pickFirstPipe start = do
  grid <- lift ask
  let points = M.keys $ gridOrthogonalNeighbours grid start
  MaybeT $ headMay <$> filterM connectsToStart points
  where connectsToStart :: Point -> Reader (Grid Char) Bool
        connectsToStart point = do
          validNeighbours <- getValidNeighbours point
          pure $ start `elem` validNeighbours

-- This could maybe use recursion-schemes ana if I could figure out how to make a baseFunctor
-- out of Data.Sequence
traverseLoop :: Point -> Point -> Reader (Grid Char) (Seq Point)
traverseLoop start firstPipe = do
  go $ Seq.fromList [start, firstPipe]
  where go path@(rest :|> mostRecent :|> current)
                      | current == start = pure path
                      | otherwise = do
                            validNeighbours <- getValidNeighbours current
                                        --Each pipe connects to two places, so there should only be one which is not the
                                        --one we just came from
                            let [next] = filter (/= mostRecent) validNeighbours
                                    in go $ path |> next
        go (Empty :|> thing) = error "Sequence with fewer than two elements. This should not be possible"
        go Empty = error "Empty sequence. This should not be possible"

--Just looked at the puzzle input and there are only two points which connect to the S. So I'm not really sure what the other
--pipes are for. Can I write this just assuming that the S only connects to two valid pipes?

insidePoints :: Seq Point -> (Reader (Grid Char)) [Point]
insidePoints path = do
  rows <- asks toRows
  let insideAccordingToRows = concatMap (`processRow` path) rows
  traceShowM "foobar"
  columns <- asks toColumns
  let insideAccordingToColumns = concatMap (`processColumn` path) columns
  pure $ traceShow ("rows:" ++ show insideAccordingToRows ++ " columns: " ++ show insideAccordingToColumns) $ insideAccordingToRows `intersect` insideAccordingToColumns

processRow :: [(Point, Char)] -> Seq Point -> [Point]
processRow row foundPath = snd $ foldl process (False, []) row
  where process current@(isInside, pointsFoundSoFar) (point, char)
          | char == '|'  && point `S.member` foundPathAsSet = (not isInside, pointsFoundSoFar)
          | char == '.'  && isInside = (isInside, point : pointsFoundSoFar)
          | otherwise = current
        foundPathAsSet = S.fromList $ toList foundPath
-- this returns an empty list which is concerning. BUT also we need to consider L, J, 7 and F as vertical pipes. We should probably also worry about S

processColumn :: [(Point, Char)] -> Seq Point -> [Point]
processColumn column foundPath = snd $ foldl process (False, []) column
  where process current@(isInside, pointsFoundSoFar) (point, char)
          | char == '-'  && point `S.member` foundPathAsSet = (not isInside, pointsFoundSoFar)
          | char == '.'  && isInside = (isInside, point : pointsFoundSoFar)
          | otherwise = current
        foundPathAsSet = S.fromList $ toList foundPath
-- this returns an empty list which is concerning. BUT also we need to consider L, J, 7 and F as vertical pipes. We should probably also worry about S

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

--'S' as a vertical pipe is not strictly true
isVerticalPipe :: Char -> Bool
isVerticalPipe = flip elem ['|', 'L', '7', 'F', 'J', 'S']
