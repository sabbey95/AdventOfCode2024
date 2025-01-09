module Solutions.Day10
  ( aoc10,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, gridNeighbours, gridOrthogonalNeighbours)
import Common.ListUtils (singleton)
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Trifecta (CharParsing (anyChar), Parser, some)
import Common.Debugging (traceLns)

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = M.map (read . singleton) . enumerateMultilineStringToVectorMap <$> some anyChar

part1 :: Grid Int -> Int
part1 = solve scoreTrail
  where
    scoreTrail = S.size . S.filter (\(_, v) -> v == 9) . S.unions . map S.fromList

part2 :: Grid Int -> Int
part2 = solve length


solve :: ([[(Point, Int)]] -> Int) -> Grid Int -> Int
solve scoreTrail grid = sum $ map (scoreTrail . expandPath grid . singleton . singleton ) trailHeads
  where
    trailHeads = M.toList $ M.filter (== 0) grid

expandPath :: Grid Int -> [[(Point, Int)]] -> [[(Point, Int)]]
expandPath grid paths = if null paths || length (head paths) == 10 then paths else expandPath grid withOthers
  where
    withOthers = concatMap getNextStep paths
    getNextStep :: [(Point, Int)] -> [[(Point, Int)]]
    getNextStep path = map (\n -> path ++ [n]) neighbours
      where
        previous = last path
        neighbours = M.toList . M.filter (== (snd previous + 1)) $ gridOrthogonalNeighbours grid (fst previous)

