module Solutions.Day4
  ( aoc4,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Map as M
import Text.Trifecta (Parser, Parsing (eof), letter, many, some, token, try)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

data GridPoint = X | M | A | S | Space deriving (Enum, Eq)

type Grid = M.Map (Int, Int) GridPoint

parseInput :: Parser Grid
parseInput = do
  gridLines <- some $ token $ some parseGridPoint
  let xCoords = length (head gridLines)
  let yCoords = length gridLines
  let coords = [(x, y) | y <- [0 .. yCoords - 1], x <- [0 .. xCoords - 1]]
  pure . M.fromList $ zip coords (concat gridLines)

parseGridPoint :: Parser GridPoint
parseGridPoint = do
  c <- letter
  case c of
    'X' -> pure X
    'M' -> pure M
    'A' -> pure A
    'S' -> pure S
    c -> fail ("Unrecognised char: " ++ [c])

type Patterns = [Pattern]

type Pattern = M.Map (Int, Int) GridPoint

part1 :: Grid -> Int
part1 = solve part1Patterns

solve :: Patterns -> Grid -> Int
solve patterns g = sum . map (countXmas g patterns) $ M.keys g

countXmas :: Grid -> Patterns -> (Int, Int) -> Int
countXmas grid patterns p = length $ filter (matchesPattern grid p) patterns

matchesPattern :: Grid -> (Int, Int) -> Pattern -> Bool
matchesPattern grid (x, y) pattern = all (\((dx, dy), gp) -> gp == M.findWithDefault Space (x + dx, y + dy) grid) $ M.toList pattern

part1Patterns :: Patterns
part1Patterns = map (\d -> M.fromList $ zip (takeLetters 0 d) [X, M, A, S]) directions

takeLetters :: Int -> (Int, Int) -> [(Int, Int)]
takeLetters 4 _ = []
takeLetters n (dx, dy) = (n * dx, n * dy) : takeLetters (n + 1) (dx, dy)

directions :: [(Int, Int)]
directions = [(x, y) | y <- [-1 .. 1], x <- [-1 .. 1], (x, y) /= (0, 0)]

part2 :: Grid -> Int
part2 = solve part2Patterns

part2Patterns :: Patterns
part2Patterns = allRotations 0 basePattern

basePattern :: Pattern
basePattern = M.fromList [((0, 0), M), ((0, 2), M), ((1, 1), A), ((2, 0), S), ((2, 2), S)]

rotatePattern :: Pattern -> Pattern
rotatePattern = M.mapKeys (\(x, y) -> (-y, x))

allRotations :: Int -> Pattern -> [Pattern]
allRotations 3 p = [p]
allRotations n p = p : allRotations (n + 1) (rotatePattern p)