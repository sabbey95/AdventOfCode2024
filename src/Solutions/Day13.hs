{-# LANGUAGE TupleSections #-}
module Solutions.Day13
  ( aoc13
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, some, optional, newline, CharParsing (string, char), integer, Errable (raiseErr))
import Common.Debugging (traceLns)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInput part2

data ClawMachine = MkClawMachine {
  buttonA:: Button,
  buttonB:: Button,
  prizeX:: Integer,
  prizeY:: Integer
} deriving (Show)

data Button = MkButton {
  x::Integer,
  y::Integer
} deriving (Show)

parseInput :: Parser [ClawMachine]
parseInput = some parseClawMachine

parseClawMachine :: Parser ClawMachine
parseClawMachine = do
          buttonA <- parseButton 'A'
          buttonB <- parseButton 'B'
          (x, y) <- parsePrizeLine
          optional newline
          pure $ MkClawMachine buttonA buttonB x y

parsePrizeLine :: Parser (Integer, Integer)
parsePrizeLine = do
  string "Prize: X="
  x <- integer
  string ", Y="
  (x,) <$> integer


parseButton :: Char -> Parser Button
parseButton c = do
  string "Button "
  char c
  string ": X+"
  x <- integer
  string ", Y+"
  MkButton x <$> integer


part1 :: [ClawMachine] -> Integer
part1 =  sum . map solveMinimumTokens

pressButton :: Integer -> Button -> (Integer, Integer)
pressButton n (MkButton x y) = (x * n, y * n)

part2 :: [ClawMachine] -> Integer
part2 = sum . map (solveMinimumTokens . increaseLimits)

increaseLimits :: ClawMachine -> ClawMachine
increaseLimits (MkClawMachine buttonA buttonB prizeX prizeY) = MkClawMachine buttonA buttonB (prizeX + 10000000000000) (prizeY + 10000000000000)

-- Did some simultaneous equations by hand
solveMinimumTokens :: ClawMachine -> Integer
solveMinimumTokens (MkClawMachine (MkButton aX aY) (MkButton bX bY) prizeX prizeY) 
        |  (a * aX + b * bX == prizeX) && (a * aY + b * bY == prizeY) = 3 * a + b
        |  otherwise = 0
      where
        numerator = fromInteger prizeY - fromInteger (prizeX * aY) / fromInteger aX
        denominator = fromInteger bY - fromInteger (bX * aY) / fromInteger aX
        bCount = numerator / denominator
        aCount = (fromInteger prizeX - (bCount * fromInteger bX)) / fromInteger aX
        (a, b) = (round aCount,round bCount)