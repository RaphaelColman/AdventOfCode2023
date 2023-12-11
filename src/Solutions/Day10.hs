module Solutions.Day10
    ( aoc10
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.Geometry                 (Grid, Point,
                                                  enumerateMultilineString,
                                                  enumerateMultilineStringToVectorMap)
import           Control.Applicative.Combinators (some)
import           Control.Monad.RWS               (MonadReader (ask),
                                                  MonadTrans (lift), asks)
import           Control.Monad.Reader            (Reader, ReaderT (runReaderT))
import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT))
import           Control.Monad.Trans.Reader      (ReaderT (..))
import           Data.Function                   ((&))
import qualified Data.Map                        as M
import           Linear.V2                       (V2)
import           Safe                            (headMay)
import           Text.Trifecta                   (CharParsing (anyChar), Parser)

data Cardinal = N | S | E | W deriving (Bounded, Enum, Eq, Ord, Show)

aoc10 :: IO ()
aoc10 = do
  printTestSolutions 10 $ MkAoCSolution parseInput part1
  --printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 = findStart

part2 :: String -> String
part2 = undefined

findStart :: Grid Char -> Maybe (Point, Char)
findStart grid = let filtered = M.filter (=='S') grid
                 in M.toList filtered & headMay

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


getValidNeighbours :: Point -> MaybeT (Reader (Grid Char)) [Point]
getValidNeighbours p = do
  MaybeT $ do
    grid <- ask
    let a = M.lookup p grid
    pure undefined
  pure undefined
