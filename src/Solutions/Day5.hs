{-# LANGUAGE TupleSections #-}
module Solutions.Day5
  ( aoc5
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, newline, try, commaSep, integer, some, token, CharParsing (char), integer', many, manyTill, Parsing (eof))
import Data.List (elemIndex, partition)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Common.Debugging (traceLns)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

data Input =
  MkInput {
  rules:: [(Integer, Integer)],
  updates:: [[Integer]]
} deriving (Show)

parseInput :: Parser Input
parseInput = do
  rules <- some $ token (try parseRule)
  MkInput rules <$> manyTill (token (commaSep integer)) eof

parseRule :: Parser (Integer, Integer)
parseRule = do
  x <- integer
  char '|'
  (x,) <$> integer

part1 :: Input -> Integer
part1 input = sum . map middle . filter (isValid (rules input)) $ updates input

middle :: [Integer] -> Integer
middle [] = 0
middle x = x!!(length x `div` 2)

isValid :: [(Integer, Integer)] -> [Integer] -> Bool
isValid rules update = snd $ foldl go ([], True) update
        where
              go :: ([Integer], Bool) -> Integer -> ([Integer], Bool)
              go (previous, False) _ = ([], False)
              go (previous, True) current = (previous ++ [current], not . any ((`elem` previous) . snd) $ filter (\(x, _) -> x == current) rules)


part2 :: Input -> Integer
part2 input = sum . map (middle . correctlyOrder (rules input)) . filter (not . isValid (rules input)) $ updates input


correctlyOrder :: [(Integer, Integer)] -> [Integer] -> [Integer]
correctlyOrder rules [] = []
correctlyOrder rules update = firstOne: correctlyOrder rules (filter (/=firstOne) update)
  where
    includedRules = filter (\(x, y) -> x `elem` update && y `elem` update) rules
    ys = map snd includedRules
    (initialRules, otherRules) = partition (\(x, _) -> x `notElem` ys) includedRules
    firstOne = if null initialRules then head update  else fst $ head initialRules
