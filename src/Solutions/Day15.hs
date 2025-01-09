{-# LANGUAGE TupleSections #-}

module Solutions.Day15
  ( aoc15,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Linear (V2 (..))
import Text.Trifecta (CharParsing (anyChar), Parser, many, oneOf, some)
import Common.Debugging (traceLns)
import Text.Regex.TDFA.Common (Instructions(Instructions))

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printSolutions 15 $ MkAoCSolution parseInput part2

data Instruction = North | East | South | West deriving (Enum, Show, Eq)

data Input = MkInput
  { warehouse :: Grid Char,
    instructions :: [Instruction]
  }
  deriving (Show)

parseInput :: Parser Input
parseInput = do
  warehouse <- enumerateMultilineStringToVectorMap . sortOutString <$> some (oneOf ['#', '\n', '.', 'O', '@'])
  MkInput warehouse . catMaybes <$> some parseInstruction

sortOutString :: String -> String
sortOutString allChars = intercalate ['\n'] neededLines
  where
    allLines = lines allChars
    sideLength = length (head allLines)
    neededLines = filter (\l -> length l == sideLength) allLines

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  c <- anyChar
  case c of
    '^' -> pure $ Just North
    '>' -> pure $ Just East
    'v' -> pure $ Just South
    '<' -> pure $ Just West
    c -> pure Nothing

part1 :: Input -> Int
part1 (MkInput grid instructions) = score $ foldl moveTile grid instructions

score :: Grid Char -> Int
score = sum . map (\(V2 x y) -> x + 100 * y) . M.keys . M.filter (`elem` "O[")

moveTile :: Grid Char -> Instruction -> Grid Char
moveTile grid instruction = M.union (M.fromList newSquares) $ M.union (M.fromList (map (,'.') originalSquares)) grid
  where
    currentSpace = head . M.keys $ M.filter (== '@') grid
    direction = getDirection instruction
    (originalSquares, newSquares) = getMoves (currentSpace, '@') direction grid ([], [])

getMoves :: (Point, Char) -> (Int, Int) -> Grid Char -> ([Point], [(Point, Char)]) -> ([Point], [(Point, Char)])
getMoves (V2 x y, currentChar) (dx, dy) grid (oldPoints, newPoints) =
  case spot of
    '#' -> ([], [])
    'O' -> getMoves (nextSpot, spot) (dx, dy) grid newState
    '[' -> if dy == 0 then getMoves (nextSpot, spot) (dx, dy) grid newState else checkVertically
    ']' -> if dy == 0 then getMoves (nextSpot, spot) (dx, dy) grid newState else checkVertically
    _ -> newState
  where
    nextSpot = V2 (x + dx) (y + dy)
    spot = M.findWithDefault '.' nextSpot grid
    newState = (V2 x y : oldPoints, (nextSpot, currentChar) : newPoints)
    checkVertically :: ([Point], [(Point, Char)])
    checkVertically = if null myNext || null otherNext then ([], []) else (myPrevious ++ otherPrevious, myNext ++ otherNext)
      where
            otherBoxSide = findOtherBoxSide spot nextSpot
            (myPrevious, myNext) = getMoves (nextSpot, spot) (dx, dy) grid newState
            (otherPrevious, otherNext) = getMoves otherBoxSide (dx, dy) grid newState

findOtherBoxSide :: Char -> Point -> (Point, Char)
findOtherBoxSide '[' (V2 x y) = (V2 (x + 1) y, ']')
findOtherBoxSide _ (V2 x y) = (V2 (x - 1) y, '[')



getDirection :: Instruction -> (Int, Int)
getDirection North = (0, -1)
getDirection South = (0, 1)
getDirection East = (1, 0)
getDirection West = (-1, 0)

part2 :: Input -> Int
part2 (MkInput grid instructions) = score $ foldl moveTile (expandGrid grid) instructions

expandGrid :: Grid Char -> Grid Char
expandGrid = M.fromList . concatMap expandPoint . M.toList

expandPoint :: (Point, Char) -> [(Point, Char)]
expandPoint (V2 x y, c) = zip points chars
    where
      points = [V2 (2 * x) y, V2 (2 * x + 1) y]
      chars = case c of
                'O' -> "[]"
                '@' -> "@."
                otherC -> [otherC, otherC]