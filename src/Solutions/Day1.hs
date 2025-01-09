module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails, sort)
import           Text.Trifecta       (Parser, TokenParsing (token), integer, whiteSpace,
                                      some)
import Common.ListUtils (window3, window2)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Locations = [(Integer, Integer)]

parseInput :: Parser Locations
parseInput = some $ token parseLocation

parseLocation :: Parser (Integer, Integer)
parseLocation = do
    i <- integer
    whiteSpace
    j <- integer
    pure (i, j)

part1 :: Locations -> Integer
part1 locations = sum (zipWith (curry dif) (sortList fst locations) (sortList snd locations))

sortList ::  ((Integer, Integer) -> Integer) -> Locations -> [Integer]
sortList fn = sort . map fn

dif:: (Integer, Integer) -> Integer
dif (a, b) = abs (b - a)

part2 :: Locations -> Integer
part2 locations = sum $ map (\n -> n * countOccurences n right) left
    where
      left = sortList fst locations
      right = sortList snd locations

countOccurences :: Eq a => a -> [a] -> Integer
countOccurences n = toInteger. length . filter (==n)