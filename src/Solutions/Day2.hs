{-# LANGUAGE RecordWildCards #-}
module Solutions.Day2
    ( aoc2
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative.Combinators (some)
import qualified Data.Map.Strict                 as M
import           Debug.Trace
import           Text.Parser.Char                (newline)
import           Text.Trifecta                   (CharParsing (string), Parser,
                                                  commaSep, integer, letter,
                                                  semiSep)

data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Ord, Show)
type Set = M.Map Colour Integer
data Game
  = MkGame
      { _label :: !Integer
      , _sets  :: ![Set]
      }
  deriving (Eq, Show)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

parseInput :: Parser [Game]
parseInput = some $ parseGame <* newline

parseGame :: Parser Game
parseGame = do
  label <- string "Game " *> integer <* string ": "
  sets <- fmap (map M.fromList) $ semiSep $ commaSep parseCubes
  pure $ MkGame label sets

parseCubes :: Parser (Colour, Integer)
parseCubes = do
  num <- integer
  c <- some letter
  case c of
    "red"   -> return (Red, num)
    "green" -> return (Green, num)
    "blue"  -> return (Blue, num)
    _       -> fail $ "Invalid colour: " ++ c

part1 :: [Game] -> Integer
part1 = sum . map _label . filter (possibleGame testSet)
  where testSet = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

part2 :: [Game] -> Integer
part2 = sum . map (M.foldr (*) 1 . fewestCubes)

possibleGame :: Set -> Game -> Bool
possibleGame set MkGame{..} = all (possible set) _sets
  where possible test cubes = all ((>=0) . snd) $ M.toList $ M.unionWith (-) test cubes

fewestCubes :: Game -> Set
fewestCubes MkGame{..} = M.unionsWith max _sets
