{-# LANGUAGE TupleSections #-}
module Solutions.Day21
  ( aoc21,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry (Grid, Point)
import qualified Data.Map as M
import Linear (V2 (..), R1 (_x), R2 (_y))
import Text.Trifecta (CharParsing (anyChar), Parser, TokenParsing (token), char, count, many, some)
import Common.Debugging (traceLns)
import Control.Lens ((^.))
import Data.List (group)

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token (count 4 anyChar)

numberPad :: Grid Char
numberPad =
  M.fromList
    [ (V2 0 0, '7'),
      (V2 1 0, '8'),
      (V2 2 0, '9'),
      (V2 0 1, '4'),
      (V2 1 1, '5'),
      (V2 2 1, '6'),
      (V2 0 2, '1'),
      (V2 1 2, '2'),
      (V2 2 2, '3'),
      (V2 1 3, '0'),
      (V2 2 3, 'A')
    ]


robotPad :: Grid Char
robotPad =
  M.fromList
    [ (V2 1 0, '^'),
      (V2 2 0, 'A'),
      (V2 0 1, '<'),
      (V2 1 1, 'v'),
      (V2 2 1, '>')
    ]

part1 :: [String] -> Int
part1 = sum . map (\s -> complexity s $ finalInstructions s 2)

complexity :: String -> Int -> Int
complexity original command = value * command
  where value = read (take 3 original)

finalInstructions:: String -> Int -> Int
finalInstructions s n =  sum . M.elems $ foldl go (spaces numberPadMoves) [0..(n-1)]
  where
    numberPadMoves = head $ getMoves 'A' numberPad s
    spaces s =  M.fromListWith (+) $ map (,1) (('A',head s):toPairs s)
    go :: M.Map (Char, Char) Int -> Int -> M.Map (Char, Char) Int
    go a _ = foldl go2 M.empty listyBoi
      where
        listyBoi = M.toList a
        go2 :: M.Map (Char, Char) Int -> ((Char, Char), Int) -> M.Map (Char, Char) Int
        go2 previous ((cPrevious, c), i) = M.unionWith (+) previous (M.map (*i) $ spaces (head $ getMoves cPrevious robotPad [c]))

toPairs :: [Char] -> [(Char, Char)]
toPairs [] = []
toPairs [x] = []
toPairs (x : y : xs) = (x, y) : toPairs (y:xs)

getMoves :: Char-> Grid Char -> String -> [String]
getMoves cursor pad = snd . foldl go (start, [""])
    where
      start = getLocation pad cursor
      go :: (Point, [String]) -> Char -> (Point, [String])
      go (current, stacks) c
          | current == destination = (current, map (++"A") stacks)
          | otherwise = (destination, concatMap (\s -> map (\s2 -> s ++ s2 ++ "A") newStacks) stacks)
          where
            destination = getLocation pad c
            (V2 xC yC) = current
            (V2 xD yD) = destination
            xMoves = range xC xD
            yMoves = range yC yD
            xFirst = map (`V2` yC) xMoves ++ map (V2 xD) yMoves
            yFirst = map (V2 xC) yMoves ++  map (`V2` yD) xMoves
            moves = filter (all (\a -> a `elem` M.keys pad)) [xFirst, yFirst]
            allNewStacks = map getNewStack moves
            newStacks = case allNewStacks of
                          [x, y] -> if head x == '<' then [x] else [y]
                          l -> l

getNewStack :: [V2 Int] -> String
getNewStack (a:(b:others)) = newChar ++ getNewStack ( b:others)
  where
    dif = (b ^. _x - a ^. _x, b ^. _y - a ^. _y )
    newChar = case dif of
              (0,1) -> "v"
              (1,0) -> ">"
              (0,-1) -> "^"
              (-1,0) -> "<"
              _ -> ""
getNewStack _ = ""

getLocation :: Grid Char -> Char -> Point
getLocation pad c = head . M.keys $ M.filter (==c) pad

range:: Int -> Int -> [Int]
range from to
  | from >= to = reverse [to..from]
  | otherwise = [from..to]

part2 :: [String] -> Int
part2 = sum . map (\s -> complexity s $ finalInstructions s 25)
