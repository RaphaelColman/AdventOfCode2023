{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day1
    ( aoc1
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Common.ListUtils                (window2, window3)
import           Control.Applicative             ((<|>))
import           Control.Applicative.Combinators (skipManyTill)
import           Control.Monad.Combinators       (skipMany, skipSome)
import           Data.Char                       (isDigit)
import           Data.Foldable                   (find)
import           Data.Functor                    (($>))
import           Data.List                       (tails)
import           Data.Maybe                      (catMaybes)
import           Text.Parser.Char                (digit, letter)
import           Text.Parser.Combinators         (endBy, sepBy, try)
import           Text.Trifecta                   (CharParsing (anyChar, char, string),
                                                  Parser, TokenParsing (token),
                                                  alphaNum, choice, integer,
                                                  manyTill, some, whiteSpace)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

parseInput :: Parser [[Char]]
parseInput = some $ token $ some alphaNum

part1 :: [String] -> Maybe Integer
part1 lines = sum <$> traverse resolveLine lines

part2 :: Traversable t => t String -> Maybe Integer
part2 lines = sum <$> traverse (resolveLine . convertLine) lines

resolveLine :: String -> Maybe Integer
resolveLine s = do
  firstDigit <- find isDigit s
  lastDigit <- find isDigit $ reverse s
  pure $ read [firstDigit, lastDigit]

convertLine :: String -> String
convertLine = go
  where go ('o':'n':'e':rest)         = '1':go ('e':rest)
        go ('t':'w':'o':rest)         = '2':go ('o':rest)
        go ('t':'h':'r':'e':'e':rest) = '3':go ('e':rest)
        go ('f':'o':'u':'r':rest)     = '4':go ('r':rest)
        go ('f':'i':'v':'e':rest)     = '5':go ('e':rest)
        go ('s':'i':'x':rest)         = '6':go ('x':rest)
        go ('s':'e':'v':'e':'n':rest) = '7':go ('e':rest)
        go ('e':'i':'g':'h':'t':rest) = '8':go ('t':rest)
        go ('n':'i':'n':'e':rest)     = '9':go ('e':rest)
        go (x:rest)                   = x:go rest
        go []                         = []
