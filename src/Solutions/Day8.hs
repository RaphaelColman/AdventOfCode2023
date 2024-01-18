{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solutions.Day8
    ( aoc8
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Data.Foldable           (Foldable (foldl'))
import           Data.Functor.Foldable
import qualified Data.Map.Strict         as M
import           Text.Parser.Char        (char)
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (Parser, alphaNum, commaSep, letter,
                                          parens, whiteSpace)

aoc8 :: IO ()
aoc8 = do
  --printTestSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

data Direction = Right' | Left' deriving (Bounded, Enum, Eq, Ord, Show)
type Node = String
type CamelMap = M.Map Node (Node, Node) -- (Left, Right)

parseCamelMap :: Parser CamelMap
parseCamelMap = do
  M.fromList <$> some parseMapEntry

parseMapEntry :: Parser (Node, (Node, Node))
parseMapEntry = do
  node <- parseNode
  whiteSpace >> char '=' >> whiteSpace
  [left, right] <- parens $ commaSep parseNode
  pure (node, (left, right))

parseNode :: Parser Node
parseNode = some alphaNum

parseDirections :: Parser [Direction]
parseDirections = some parseDirection
  where parseDirection :: Parser Direction
        parseDirection = do
          dir <- letter
          case dir of
            'R' -> return Right'
            'L' -> return Left'
            _   -> fail "Invalid direction"

parseInput :: Parser ([Direction], CamelMap)
parseInput = do
  directions <- parseDirections
  whiteSpace
  map <- parseCamelMap
  pure (directions, map)

--part1 :: String -> String
--part1 = undefined

part2 :: ([Direction], CamelMap) -> Integer
part2 (directions, camelMap) = foldr1 lcm lengths
  where startNodes = filter ((== 'A') . last) $ M.keys camelMap
        lengths = map (\n -> pathLength camelMap n directions) startNodes

-- Would be nice to find a recursion scheme here instead of explicit recursion
pathLength :: CamelMap -> Node -> [Direction] -> Integer
pathLength map start directions = go start (cycle directions) 0
  where nextNode :: Node -> Direction -> Node
        nextNode node direction = let (left, right) = map M.! node
                        in case direction of
                          Right' -> right
                          Left'  -> left
        go current (nextDirection:rest) count --This non-exhaustive pattern match is fine becuase we've cycled directions to be infinite
          | last current == 'Z' = count
            | otherwise = go (nextNode current nextDirection) rest (count+1)
